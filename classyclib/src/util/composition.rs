pub trait Pipe: Sized {
    fn pipe<R>(self, f: impl FnOnce(Self) -> R) -> R {
        f(self)
    }
}

impl<T> Pipe for T {}

pub trait Composable<In, Out> {
    fn compose<Res>(self, f: impl FnOnce(Out) -> Res) -> impl FnOnce(In) -> Res;
}

impl<In, Out, F: FnOnce(In) -> Out> Composable<In, Out> for F {
    fn compose<Res>(self, f: impl FnOnce(Out) -> Res) -> impl FnOnce(In) -> Res {
        |x| f(self(x))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pipe() {
        let result = 1.pipe(|x| x + 1);
        assert_eq!(result, 2);
    }

    #[test]
    fn test_compose() {
        let result = (|x| x + 1).compose(|x| x * 2)(1);
        assert_eq!(result, 4);
    }
}
