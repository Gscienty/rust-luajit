pub(crate) struct Logger;

impl log::Log for Logger {
    fn flush(&self) {}

    fn enabled(&self, _metadata: &log::Metadata) -> bool {
        true
    }

    fn log(&self, record: &log::Record) {
        println!("{}:{} - {}", record.level(), record.target(), record.args())
    }
}
