#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    start: usize,
    end: usize,
    file_id: FileId,
}

impl Span {
    #[must_use]
    pub fn zero(file_id: FileId) -> Self {
        chumsky::span::Span::new(file_id, 0..0)
    }
}

impl chumsky::span::Span for Span {
    type Context = FileId;

    type Offset = usize;

    fn new(context: Self::Context, range: core::ops::Range<Self::Offset>) -> Self {
        Self {
            start: range.start,
            end: range.end,
            file_id: context,
        }
    }

    fn context(&self) -> Self::Context {
        self.file_id
    }

    fn start(&self) -> Self::Offset {
        self.start
    }

    fn end(&self) -> Self::Offset {
        self.end
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct FileId(pub usize);

impl FileId {
    #[must_use]
    pub const fn new(id: usize) -> Self {
        Self(id)
    }
}

#[derive(Clone, Copy, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Spanned<T>(pub T, pub Span);

impl<T> Spanned<T> {
    pub const fn new(value: T, span: Span) -> Self {
        Self(value, span)
    }

    pub fn boxed(self) -> Spanned<Box<T>> {
        Spanned(Box::new(self.0), self.1)
    }
}
