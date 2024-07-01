use crate::{
    parser::ast::{BinaryOpKind, FunctionId, Type as AstType, UnaryOpKind, VariableId},
    typechecker::{
        typed_ast::{
            self, ExpressionData, Function, FunctionParameter, TypedExpression, TypedProgram,
        },
        validation::MAIN_FUNCTION_NAME,
    },
};
use cranelift::{
    codegen::Context,
    module::{Linkage, Module},
    object::{ObjectBuilder, ObjectModule, ObjectProduct},
    prelude::*,
};
use rustc_hash::FxHashMap;
use target_lexicon::Triple;

pub fn codegen(db: &dyn crate::Db, program: TypedProgram, triple: Triple) -> Option<ObjectProduct> {
    let mut settings = settings::builder();
    settings.enable("is_pic").unwrap();

    let isa_builder = cranelift::codegen::isa::lookup(triple).unwrap();

    let isa = isa_builder.finish(settings::Flags::new(settings)).unwrap();

    let object_builder =
        ObjectBuilder::new(isa, b"main", cranelift::module::default_libcall_names()).unwrap();

    let mut object_module = ObjectModule::new(object_builder);

    let mut codegen = Codegen::new(&mut object_module, db);

    codegen.compile(program);

    Some(object_module.finish())
}

struct Codegen<'a, 'db> {
    db: &'db dyn crate::Db,
    builder_ctx: FunctionBuilderContext,
    ctx: Context,
    module: &'a mut dyn Module,
}

impl<'a, 'db> Codegen<'a, 'db> {
    fn new(module: &'a mut dyn Module, db: &'db dyn crate::Db) -> Self {
        Self {
            db,
            builder_ctx: FunctionBuilderContext::new(),
            ctx: module.make_context(),
            module,
        }
    }

    fn compile(&mut self, program: TypedProgram<'db>) {
        let functions_map = program
            .functions(self.db)
            .iter()
            .map(|function| (function.name(self.db), *function))
            .collect();

        for function in program.functions(self.db) {
            self.compile_function(*function, &functions_map);
        }
    }

    fn compile_function(
        &mut self,
        function: typed_ast::Function<'db>,
        functions: &FxHashMap<FunctionId<'db>, Function<'db>>,
    ) {
        for param in function.params(self.db) {
            self.ctx
                .func
                .signature
                .params
                .push(AbiParam::new(param.type_.try_into().unwrap()));
        }

        self.ctx.func.signature.returns.push(AbiParam::new(
            function.return_type(self.db).try_into().unwrap(),
        ));

        let mut builder = FunctionBuilder::new(&mut self.ctx.func, &mut self.builder_ctx);

        let entry_block = builder.create_block();

        builder.append_block_params_for_function_params(entry_block);

        builder.switch_to_block(entry_block);

        builder.seal_block(entry_block);

        let mut translator = FunctionTranslator::new(self.db, builder, self.module, functions);

        translator.translate(
            function.body(self.db),
            entry_block,
            function.params(self.db),
        );

        translator.builder.finalize();

        let linkage = match function.name(self.db).text(self.db).as_str() {
            MAIN_FUNCTION_NAME => Linkage::Export,
            _ => Linkage::Local,
        };

        let id = self
            .module
            .declare_function(
                function.name(self.db).text(self.db),
                linkage,
                &self.ctx.func.signature,
            )
            .unwrap();

        self.module.define_function(id, &mut self.ctx).unwrap();

        self.module.clear_context(&mut self.ctx);
    }
}

struct FunctionTranslator<'a, 'b, 'db> {
    db: &'db dyn crate::Db,
    builder: FunctionBuilder<'a>,
    module: &'a mut dyn Module,
    var_idx: usize,
    vars: FxHashMap<VariableId<'db>, Variable>,
    functions: &'b FxHashMap<FunctionId<'db>, Function<'db>>,
}

impl<'a, 'b, 'db> FunctionTranslator<'a, 'b, 'db> {
    fn new(
        db: &'db dyn crate::Db,
        builder: FunctionBuilder<'a>,
        module: &'a mut dyn Module,
        functions: &'b FxHashMap<FunctionId<'db>, Function<'db>>,
    ) -> Self {
        Self {
            db,
            builder,
            module,
            var_idx: 0,
            vars: FxHashMap::default(),
            functions,
        }
    }

    fn translate(
        &mut self,
        body: &TypedExpression<'db>,
        entry_block: Block,
        params: &[FunctionParameter<'db>],
    ) {
        for (idx, param) in params.iter().enumerate() {
            let var = self.new_var();

            self.builder
                .declare_var(var, param.type_.try_into().unwrap());

            self.builder
                .def_var(var, self.builder.block_params(entry_block)[idx]);

            self.vars.insert(param.name, var);
        }

        let value = self.translate_expr(body);

        self.builder.ins().return_(&[value]);
    }

    fn translate_expr(&mut self, expr: &TypedExpression<'db>) -> Value {
        match &expr.data {
            ExpressionData::Integer(value) => {
                self.builder.ins().iconst(types::I32, i64::from(*value))
            }
            ExpressionData::Float(value) => self.builder.ins().f32const(value.0),
            ExpressionData::Variable(name) => self.builder.use_var(self.vars[name]),
            ExpressionData::Call(function_id, call_args) => {
                let mut sig = self.module.make_signature();

                let function = self.functions.get(function_id).unwrap();

                for param in function.params(self.db) {
                    sig.params
                        .push(AbiParam::new(param.type_.try_into().unwrap()));
                }

                sig.returns.push(AbiParam::new(
                    function.return_type(self.db).try_into().unwrap(),
                ));

                let callee = self
                    .module
                    .declare_function(function.name(self.db).text(self.db), Linkage::Import, &sig)
                    .unwrap();

                let local_callee = self.module.declare_func_in_func(callee, self.builder.func);

                let args = call_args
                    .iter()
                    .map(|arg| self.translate_expr(arg))
                    .collect::<Vec<_>>();

                let call = self.builder.ins().call(local_callee, &args);

                self.builder.inst_results(call)[0]
            }
            ExpressionData::UnaryOp(op, rhs) => {
                let rhs_ty = rhs.ty;
                let rhs = self.translate_expr(rhs);

                match rhs_ty {
                    AstType::Integer => match op.data {
                        UnaryOpKind::Negate => self.builder.ins().ineg(rhs),
                    },
                    AstType::Float => match op.data {
                        UnaryOpKind::Negate => self.builder.ins().fneg(rhs),
                    },
                    AstType::Error => unreachable!(),
                }
            }
            ExpressionData::BinaryOp(op, lhs, rhs) => {
                let lhs_ty = lhs.ty;
                let rhs_ty = rhs.ty;
                let lhs = self.translate_expr(lhs);
                let rhs = self.translate_expr(rhs);

                match (lhs_ty, rhs_ty) {
                    (AstType::Integer, AstType::Integer) => match op.data {
                        BinaryOpKind::Add => self.builder.ins().iadd(lhs, rhs),
                        BinaryOpKind::Subtract => self.builder.ins().isub(lhs, rhs),
                        BinaryOpKind::Multiply => self.builder.ins().imul(lhs, rhs),
                        BinaryOpKind::Divide => self.builder.ins().sdiv(lhs, rhs),
                    },
                    (AstType::Float, AstType::Float) => match op.data {
                        BinaryOpKind::Add => self.builder.ins().fadd(lhs, rhs),
                        BinaryOpKind::Subtract => self.builder.ins().fsub(lhs, rhs),
                        BinaryOpKind::Multiply => self.builder.ins().fmul(lhs, rhs),
                        BinaryOpKind::Divide => self.builder.ins().fdiv(lhs, rhs),
                    },
                    _ => unreachable!(),
                }
            }
        }
    }

    fn new_var(&mut self) -> Variable {
        let var = Variable::new(self.var_idx);
        self.var_idx += 1;
        var
    }
}

impl TryFrom<AstType> for Type {
    type Error = ();

    fn try_from(value: AstType) -> Result<Self, Self::Error> {
        match value {
            AstType::Error => Err(()),
            AstType::Integer => Ok(types::I32),
            AstType::Float => Ok(types::F32),
        }
    }
}
