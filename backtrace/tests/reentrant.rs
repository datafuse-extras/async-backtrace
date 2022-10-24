/// A test that a blocking threaddump does not deadlock a program when requested
/// from within a `framed` task.
mod util;
use taskdump::framed;

#[test]
fn reentrant() {
    util::model(|| util::run(outer()));
}

#[framed]
async fn outer() {
    let dump = taskdump::taskdump(true);
    pretty_assertions::assert_str_eq!(
        util::strip(dump),
        "\
╼ reentrant::outer::{{closure}} at backtrace/tests/reentrant.rs:LINE:COL"
    );
    inner().await;
}

#[framed]
async fn inner() {
    let dump = taskdump::taskdump(true);
    pretty_assertions::assert_str_eq!(
        util::strip(dump),
        "\
╼ reentrant::outer::{{closure}} at backtrace/tests/reentrant.rs:LINE:COL
  └╼ reentrant::inner::{{closure}} at backtrace/tests/reentrant.rs:LINE:COL"
    );
}
