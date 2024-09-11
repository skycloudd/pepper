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

    #[must_use]
    pub const fn range(&self) -> core::ops::Range<usize> {
        self.start..self.end
    }

    #[must_use]
    pub const fn at_last_idx(&self) -> Self {
        let last_char = self.end.saturating_sub(1);

        Self {
            start: last_char,
            end: last_char,
            file_id: self.file_id,
        }
    }

    #[must_use]
    pub fn without_start(&self, n: usize) -> Self {
        let new_start = self.start + n;

        Self {
            start: new_start,
            end: self.end.max(new_start),
            file_id: self.file_id,
        }
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

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned(f(self.0), self.1)
    }

    #[must_use]
    pub fn map_span(self, f: impl FnOnce(Span) -> Span) -> Self {
        Self(self.0, f(self.1))
    }

    pub const fn as_ref(&self) -> Spanned<&T> {
        Spanned(&self.0, self.1)
    }

    pub fn as_mut(&mut self) -> Spanned<&mut T> {
        Spanned(&mut self.0, self.1)
    }

    pub fn map_with<U>(&self, f: impl FnOnce(&T, Span) -> U) -> Spanned<U> {
        Spanned(f(&self.0, self.1), self.1)
    }
}
