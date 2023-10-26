use std::fmt::Display;

use futures::Future;

/// Produces a [`Location`] when invoked in a function body.
///
/// ```
/// use async_backtrace::{location, Location};
///
/// #[tokio::main]
/// async fn main() {
/// #   macro_rules! assert_eq { ($l:expr, $r:expr) => { debug_assert_eq!($l.len(), $r.len()); } }
///     assert_eq!(location!().to_string(), "rust_out::main::{{closure}} at src/location.rs:8:16");
///
///     async {
///         assert_eq!(location!().to_string(), "rust_out::main::{{closure}}::{{closure}} at src/location.rs:11:20");
///     }.await;
///     
///     (|| async {
///         assert_eq!(location!().to_string(), "rust_out::main::{{closure}}::{{closure}}::{{closure}} at src/location.rs:15:20");
///     })().await;
/// }
/// ```
#[macro_export]
macro_rules! location {
    () => {{
        macro_rules! fn_name {
            () => {{
                fn type_name_of_val<T: ?Sized>(_: &T) -> &'static str {
                    core::any::type_name::<T>()
                }
                type_name_of_val(&|| {})
                    .strip_suffix("::{{closure}}")
                    .unwrap()
            }};
        }

        $crate::Location::from_generated(fn_name!(), &(file!(), line!(), column!()))
    }};
    ($custom_name:expr) => {{
        $crate::Location::from_custom($custom_name, &(file!(), line!(), column!()))
    }};
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct CustomLocation {
    name: Option<String>,
    rest: &'static (&'static str, u32, u32),
}

impl CustomLocation {
    pub fn name(&self) -> Option<&str> {
        match &self.name {
            None => None,
            Some(name) => Some(name.as_str()),
        }
    }

    pub const fn file(&self) -> &str {
        self.rest.0
    }

    pub const fn line(&self) -> u32 {
        self.rest.1
    }

    pub const fn column(&self) -> u32 {
        self.rest.2
    }
}

/// A source code location in a function body.
///
/// To construct a `Location`, use [`location!()`].
#[derive(Debug, Copy, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub struct GeneratedLocation {
    name: Option<&'static str>,
    /// The file name, line number, and column number on which the surrounding
    /// function is defined.
    rest: &'static (&'static str, u32, u32),
}

impl GeneratedLocation {
    pub const fn name(&self) -> Option<&str> {
        self.name
    }

    pub const fn file(&self) -> &str {
        self.rest.0
    }

    pub const fn line(&self) -> u32 {
        self.rest.1
    }

    pub const fn column(&self) -> u32 {
        self.rest.2
    }
}

#[derive(Debug, Clone, Eq, PartialEq, Ord, PartialOrd, Hash)]
pub enum Location {
    Custom(CustomLocation),
    Generated(GeneratedLocation),
}

impl Location {
    /// **DO NOT USE!** The signature of this method may change between
    /// non-breaking releases.
    #[doc(hidden)]
    #[inline(always)]
    pub const fn from_generated(
        name: &'static str,
        rest: &'static (&'static str, u32, u32),
    ) -> Self {
        Self::Generated(GeneratedLocation {
            name: Some(name),
            rest,
        })
    }

    pub fn from_custom(name: String, rest: &'static (&'static str, u32, u32)) -> Self {
        Self::Custom(CustomLocation {
            name: Some(name),
            rest,
        })
    }

    /// Include the given future in taskdumps with this location.
    ///
    /// ## Examples
    /// ```
    /// # async fn bar() {}
    /// # async fn baz() {}
    /// async fn foo() {
    ///     async_backtrace::location!().frame(async move {
    ///         bar().await;
    ///         baz().await;
    ///     }).await
    /// }
    /// ```
    pub fn frame<F>(self, f: F) -> impl Future<Output = F::Output>
    where
        F: Future,
    {
        crate::Framed::new(f, self)
    }

    /// Produces the function name associated with this location.
    pub fn name(&self) -> Option<&str> {
        match self {
            Location::Custom(v) => v.name(),
            Location::Generated(v) => v.name(),
        }
    }

    /// Produces the file name associated with this location.
    pub const fn file(&self) -> &str {
        match self {
            Location::Custom(v) => v.file(),
            Location::Generated(v) => v.file(),
        }
    }

    /// Produces the line number associated with this location.
    pub const fn line(&self) -> u32 {
        match self {
            Location::Custom(v) => v.line(),
            Location::Generated(v) => v.line(),
        }
    }

    /// Produces the column number associated with this location.
    pub const fn column(&self) -> u32 {
        match self {
            Location::Custom(v) => v.column(),
            Location::Generated(v) => v.column(),
        }
    }
}

impl Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let file = self.file();
        let line = self.line();
        let column = self.column();
        if let Some(name) = self.name() {
            f.write_fmt(format_args!("{name} at {file}:{line}:{column}"))
        } else {
            f.write_fmt(format_args!("{file}:{line}:{column}"))
        }
    }
}
