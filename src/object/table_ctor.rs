use super::Expr;

pub(crate) struct TableCtor {
    pub(crate) lexp: Expr,
    pub(crate) texp: Expr,

    pub(crate) nh: usize,
    pub(crate) na: usize,
    pub(crate) tostore: usize,
}

impl TableCtor {
    pub(crate) fn new() -> Self {
        TableCtor {
            lexp: Expr::void(),
            texp: Expr::todo(),
            nh: 0,
            na: 0,
            tostore: 0,
        }
    }
}
