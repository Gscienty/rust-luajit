pub struct GCState {
    total: usize, // memory currently allocated
}

impl GCState {
    pub fn new() -> Self {
        GCState {
            total: 0,
        }
    }

    pub fn get_total(&self) -> usize {
        self.total
    }

    pub fn set_total(&mut self, total: usize) {
        self.total = total
    }
}