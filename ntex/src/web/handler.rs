use std::{fmt, future::Future, marker::PhantomData};

use crate::util::BoxFuture;

use super::error::ErrorRenderer;
use super::extract::FromRequest;
use super::request::WebRequest;
use super::responder::Responder;
use super::response::WebResponse;

/// Async fn handler
pub trait Handler<T, Err>
where
    Err: ErrorRenderer,
{
    type Output: Responder<Err>;

    fn call(&self, param: T) -> impl Future<Output = Self::Output>;
}

impl<F, R, Err> Handler<(), Err> for F
where
    F: AsyncFn() -> R,
    R: Responder<Err>,
    Err: ErrorRenderer,
{
    type Output = R;

    async fn call(&self, _: ()) -> R {
        (self)().await
    }
}

pub(super) trait HandlerFn<Err: ErrorRenderer>: fmt::Debug {
    fn call(
        &self,
        _: WebRequest<Err>,
    ) -> BoxFuture<'_, Result<WebResponse, Err::Container>>;
}

pub(super) struct HandlerWrapper<F, T, Err> {
    hnd: F,
    _t: PhantomData<(T, Err)>,
}

impl<F, T, Err> HandlerWrapper<F, T, Err> {
    pub(super) fn new(hnd: F) -> Self {
        HandlerWrapper {
            hnd,
            _t: PhantomData,
        }
    }
}

impl<F, T, Err> fmt::Debug for HandlerWrapper<F, T, Err> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Handler({:?})", std::any::type_name::<F>())
    }
}

impl<F, T, Err> HandlerFn<Err> for HandlerWrapper<F, T, Err>
where
    F: Handler<T, Err> + 'static,
    T: FromRequest<Err> + 'static,
    T::Error: Into<Err::Container>,
    Err: ErrorRenderer,
{
    fn call(
        &self,
        req: WebRequest<Err>,
    ) -> BoxFuture<'_, Result<WebResponse, Err::Container>> {
        Box::pin(async move {
            let (req, mut payload) = req.into_parts();
            let param = match T::from_request(&req, &mut payload).await {
                Ok(param) => param,
                Err(e) => return Ok(WebResponse::from_err::<Err, _>(e, req)),
            };

            let result = self.hnd.call(param).await;
            let response = result.respond_to(&req).await;
            Ok(WebResponse::new(response, req))
        })
    }
}

/// FromRequest trait impl for tuples
macro_rules! factory_tuple (
    {$(#[$meta:meta])* $(($T:ident, $t:ident)),+} => {
        $(#[$meta])*
        impl<Func, $($T,)+ Res, Err> Handler<($($T,)+), Err> for Func
        where Func: AsyncFn($($T,)+) -> Res,
            Res: Responder<Err>,
            Err: ErrorRenderer,
        {
            type Output = Res;

            async fn call(&self, ($($t,)+): ($($T,)+)) -> Self::Output {
                (self)($($t,)+).await
            }
        }
    }
);

#[rustfmt::skip]
mod m {
    use super::*;
    use variadics_please::all_tuples;

    // Can't use #[doc(fake_variadic)] here
    // all_tuples!(factory_tuple, 1, 16, T, t);

    impl<Func, T0, Res, Err> Handler<(T0,), Err> for Func
    where
        Func: AsyncFn(T0) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(&self, (t0,): (T0,)) -> Self::Output {
            (self)(t0).await
        }
    }
    impl<Func, T0, T1, Res, Err> Handler<(T0, T1), Err> for Func
    where
        Func: AsyncFn(T0, T1) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(&self, (t0, t1): (T0, T1)) -> Self::Output {
            (self)(t0, t1).await
        }
    }
    impl<Func, T0, T1, T2, Res, Err> Handler<(T0, T1, T2), Err> for Func
    where
        Func: AsyncFn(T0, T1, T2) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(&self, (t0, t1, t2): (T0, T1, T2)) -> Self::Output {
            (self)(t0, t1, t2).await
        }
    }
    impl<Func, T0, T1, T2, T3, Res, Err> Handler<(T0, T1, T2, T3), Err> for Func
    where
        Func: AsyncFn(T0, T1, T2, T3) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(&self, (t0, t1, t2, t3): (T0, T1, T2, T3)) -> Self::Output {
            (self)(t0, t1, t2, t3).await
        }
    }
    impl<Func, T0, T1, T2, T3, T4, Res, Err> Handler<(T0, T1, T2, T3, T4), Err> for Func
    where
        Func: AsyncFn(T0, T1, T2, T3, T4) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(&self, (t0, t1, t2, t3, t4): (T0, T1, T2, T3, T4)) -> Self::Output {
            (self)(t0, t1, t2, t3, t4).await
        }
    }
    impl<Func, T0, T1, T2, T3, T4, T5, Res, Err> Handler<(T0, T1, T2, T3, T4, T5), Err> for Func
    where
        Func: AsyncFn(T0, T1, T2, T3, T4, T5) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(
            &self,
            (t0, t1, t2, t3, t4, t5): (T0, T1, T2, T3, T4, T5),
        ) -> Self::Output {
            (self)(t0, t1, t2, t3, t4, t5).await
        }
    }
    impl<Func, T0, T1, T2, T3, T4, T5, T6, Res, Err>
        Handler<(T0, T1, T2, T3, T4, T5, T6), Err> for Func
    where
        Func: AsyncFn(T0, T1, T2, T3, T4, T5, T6) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(
            &self,
            (t0, t1, t2, t3, t4, t5, t6): (T0, T1, T2, T3, T4, T5, T6),
        ) -> Self::Output {
            (self)(t0, t1, t2, t3, t4, t5, t6).await
        }
    }
    impl<Func, T0, T1, T2, T3, T4, T5, T6, T7, Res, Err>
        Handler<(T0, T1, T2, T3, T4, T5, T6, T7), Err> for Func
    where
        Func: AsyncFn(T0, T1, T2, T3, T4, T5, T6, T7) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(
            &self,
            (t0, t1, t2, t3, t4, t5, t6, t7): (T0, T1, T2, T3, T4, T5, T6, T7),
        ) -> Self::Output {
            (self)(t0, t1, t2, t3, t4, t5, t6, t7).await
        }
    }
    impl<Func, T0, T1, T2, T3, T4, T5, T6, T7, T8, Res, Err>
        Handler<(T0, T1, T2, T3, T4, T5, T6, T7, T8), Err> for Func
    where
        Func: AsyncFn(T0, T1, T2, T3, T4, T5, T6, T7, T8) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(
            &self,
            (t0, t1, t2, t3, t4, t5, t6, t7, t8): (T0, T1, T2, T3, T4, T5, T6, T7, T8),
        ) -> Self::Output {
            (self)(t0, t1, t2, t3, t4, t5, t6, t7, t8).await
        }
    }
    impl<Func, T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, Res, Err>
        Handler<(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9), Err> for Func
    where
        Func: AsyncFn(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(
            &self,
            (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9): (
                T0,
                T1,
                T2,
                T3,
                T4,
                T5,
                T6,
                T7,
                T8,
                T9,
            ),
        ) -> Self::Output {
            (self)(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9).await
        }
    }
    impl<Func, T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, Res, Err>
        Handler<(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10), Err> for Func
    where
        Func: AsyncFn(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(
            &self,
            (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10): (
                T0,
                T1,
                T2,
                T3,
                T4,
                T5,
                T6,
                T7,
                T8,
                T9,
                T10,
            ),
        ) -> Self::Output {
            (self)(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10).await
        }
    }
    impl<Func, T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, Res, Err>
        Handler<(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11), Err> for Func
    where
        Func: AsyncFn(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(
            &self,
            (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11): (
                T0,
                T1,
                T2,
                T3,
                T4,
                T5,
                T6,
                T7,
                T8,
                T9,
                T10,
                T11,
            ),
        ) -> Self::Output {
            (self)(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11).await
        }
    }
    impl<Func, T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, Res, Err>
        Handler<(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12), Err> for Func
    where
        Func: AsyncFn(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(
            &self,
            (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12): (
                T0,
                T1,
                T2,
                T3,
                T4,
                T5,
                T6,
                T7,
                T8,
                T9,
                T10,
                T11,
                T12,
            ),
        ) -> Self::Output {
            (self)(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12).await
        }
    }
    impl<Func, T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, Res, Err>
        Handler<(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13), Err> for Func
    where
        Func: AsyncFn(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(
            &self,
            (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13): (
                T0,
                T1,
                T2,
                T3,
                T4,
                T5,
                T6,
                T7,
                T8,
                T9,
                T10,
                T11,
                T12,
                T13,
            ),
        ) -> Self::Output {
            (self)(t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13).await
        }
    }
    impl<Func, T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, Res, Err>
        Handler<
            (
                T0,
                T1,
                T2,
                T3,
                T4,
                T5,
                T6,
                T7,
                T8,
                T9,
                T10,
                T11,
                T12,
                T13,
                T14,
            ),
            Err,
        > for Func
    where
        Func:
            AsyncFn(T0, T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(
            &self,
            (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14): (
                T0,
                T1,
                T2,
                T3,
                T4,
                T5,
                T6,
                T7,
                T8,
                T9,
                T10,
                T11,
                T12,
                T13,
                T14,
            ),
        ) -> Self::Output {
            (self)(
                t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14,
            )
            .await
        }
    }
    impl<
        Func,
        T0,
        T1,
        T2,
        T3,
        T4,
        T5,
        T6,
        T7,
        T8,
        T9,
        T10,
        T11,
        T12,
        T13,
        T14,
        T15,
        Res,
        Err,
    >
        Handler<
            (
                T0,
                T1,
                T2,
                T3,
                T4,
                T5,
                T6,
                T7,
                T8,
                T9,
                T10,
                T11,
                T12,
                T13,
                T14,
                T15,
            ),
            Err,
        > for Func
    where
        Func: AsyncFn(
            T0,
            T1,
            T2,
            T3,
            T4,
            T5,
            T6,
            T7,
            T8,
            T9,
            T10,
            T11,
            T12,
            T13,
            T14,
            T15,
        ) -> Res,
        Res: Responder<Err>,
        Err: ErrorRenderer,
    {
        type Output = Res;
        async fn call(
            &self,
            (t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15): (
                T0,
                T1,
                T2,
                T3,
                T4,
                T5,
                T6,
                T7,
                T8,
                T9,
                T10,
                T11,
                T12,
                T13,
                T14,
                T15,
            ),
        ) -> Self::Output {
            (self)(
                t0, t1, t2, t3, t4, t5, t6, t7, t8, t9, t10, t11, t12, t13, t14, t15,
            )
            .await
        }
    }
}
