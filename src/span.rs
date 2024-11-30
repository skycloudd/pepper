use core::num::NonZeroUsize;

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
#[cfg_attr(test, derive(serde::Serialize))]
pub struct Span {
    start: usize,
    end: usize,
    file_id: FileId,
}

impl Span {
    pub fn zero(file_id: FileId) -> Self {
        chumsky::span::Span::new(file_id, 0..0)
    }

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
#[cfg_attr(test, derive(serde::Serialize))]
pub struct FileId(NonZeroUsize);

impl FileId {
    pub fn new(id: usize) -> Option<Self> {
        Some(Self(NonZeroUsize::new(id.checked_add(1)?)?))
    }

    pub fn get(self) -> usize {
        usize::from(self.0) - 1
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct Spanned<T>(pub T, Span);

impl<T> Spanned<T> {
    pub const fn new(value: T, span: Span) -> Self {
        Self(value, span)
    }

    pub const fn span(&self) -> Span {
        self.1
    }

    pub fn boxed(self) -> Spanned<Box<T>> {
        Spanned::new(Box::new(self.0), self.1)
    }

    pub fn map<U>(self, f: impl FnOnce(T) -> U) -> Spanned<U> {
        Spanned::new(f(self.0), self.1)
    }

    pub fn map_with_span<U>(self, f: impl FnOnce(T, Span) -> U) -> Spanned<U> {
        Spanned::new(f(self.0, self.1), self.1)
    }

    pub fn map_span(self, f: impl FnOnce(Span) -> Span) -> Self {
        Self(self.0, f(self.1))
    }

    pub const fn as_ref(&self) -> Spanned<&T> {
        Spanned::new(&self.0, self.1)
    }
}

impl<T, E> Spanned<Result<T, E>> {
    pub fn transpose(self) -> Result<Spanned<T>, E> {
        self.0.map(|value| Spanned::new(value, self.1))
    }
}

impl<T> Spanned<Box<T>> {
    pub fn unbox(self) -> Spanned<T> {
        Spanned::new(*self.0, self.1)
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

#[cfg(test)]
impl<T: serde::Serialize> serde::Serialize for Spanned<T> {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        self.0.serialize(serializer)
    }
}
