use core::num::NonZeroUsize;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
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

    #[must_use]
    pub const fn range(&self) -> core::ops::Range<usize> {
        self.start..self.end
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

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct FileId(NonZeroUsize);

impl FileId {
    #[must_use]
    pub fn new(id: usize) -> Self {
        Self(NonZeroUsize::new(id.checked_add(1).unwrap()).unwrap())
    }

    #[must_use]
    pub fn get(self) -> usize {
        usize::from(self.0) - 1
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Spanned<T>(pub T, pub Span);

impl<T> Spanned<T> {
    pub const fn new(value: T, span: Span) -> Self {
        Self(value, span)
    }

    pub fn boxed(self) -> Spanned<Box<T>> {
        Spanned(Box::new(self.0), self.1)
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned(f(self.0), self.1)
    }

    pub fn map_with_span<U>(self, f: impl FnOnce(T, Span) -> U) -> Spanned<U> {
        Spanned(f(self.0, self.1), self.1)
    }

    pub const fn as_ref(&self) -> Spanned<&T> {
        Spanned(&self.0, self.1)
    }
}

impl<T> Spanned<Option<T>> {
    pub fn transpose(self) -> Option<Spanned<T>> {
        self.0.map(|value| Spanned(value, self.1))
    }
}

impl<T, E> Spanned<Result<T, E>> {
    pub fn transpose_result(self) -> Result<Spanned<T>, E> {
        self.0.map(|value| Spanned(value, self.1))
    }
}

impl<T> Spanned<Box<T>> {
    #[must_use]
    pub fn unbox(self) -> Spanned<T> {
        Spanned(*self.0, self.1)
    }
}

impl<T: Clone> Spanned<&T> {
    pub fn cloned(self) -> Spanned<T> {
        self.map(Clone::clone)
    }
}

impl<T> core::ops::Deref for Spanned<T> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl<T> core::ops::DerefMut for Spanned<T> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        &mut self.0
    }
}
