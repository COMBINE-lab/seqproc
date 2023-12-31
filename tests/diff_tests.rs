use std::{
    error::Error,
    fmt,
    fs::{self, DirEntry},
    process::Command,
};

use assert_cmd::cargo::CommandCargoExt;
use similar_asserts::SimpleDiff;
use tempdir::TempDir;

#[derive(Debug)]
struct DiffErr;

impl fmt::Display for DiffErr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Output does not match")
    }
}

impl Error for DiffErr {}

#[test]
fn run_tests() -> Result<(), Box<dyn Error>> {
    let mut failed: bool = false;

    let tests_dir_path = &*std::env::current_dir().unwrap().join("tests");
    let test_data_dir_path = &*tests_dir_path.join("test_data");
    let fgdl_dir_path = &*tests_dir_path.join("fgdl");
    let in_dir_path = &*test_data_dir_path.join("in");
    let out_dir = TempDir::new("seqproc_test_out")?;
    let expected_out_dir_path = &*test_data_dir_path.join("expected_out");

    for entry in in_dir_path.read_dir()? {
        let entry = entry?;
        let entry_name = &*entry.file_name().into_string().unwrap();
        let fgdl = &*fgdl_dir_path.join(format!("{entry_name}.fgdl"));
        let in_1 = &*entry.path().join(format!("{entry_name}_l.fastq"));
        let in_2 = &*entry.path().join(format!("{entry_name}_r.fastq"));
        let out = &*out_dir.path().join(format!("{entry_name}.fastq"));
        let expected_out = &*expected_out_dir_path.join(format!("{entry_name}.fastq"));

        let in_dir_entries = entry.path().read_dir()?;
        let in_dir_entries: Vec<DirEntry> = in_dir_entries.collect::<Result<_, _>>()?;

        let third_file = in_dir_entries
            .into_iter()
            .find(|entry| ![in_1, in_2].contains(&&*entry.path()));

        let mut cmd = Command::cargo_bin("seqproc")?;
        if let Some(third_file) = third_file {
            cmd.args([
                "-g".as_ref(),
                fgdl,
                "-1".as_ref(),
                in_1,
                "-2".as_ref(),
                in_2,
                "-o".as_ref(),
                out,
                "-t".as_ref(),
                "6".as_ref(),
                "-a".as_ref(),
                &*third_file.path(),
            ]);
        } else {
            cmd.args([
                "-g".as_ref(),
                fgdl,
                "-1".as_ref(),
                in_1,
                "-2".as_ref(),
                in_2,
                "-o".as_ref(),
                out,
                "-t".as_ref(),
                "6".as_ref(),
            ]);
        }

        let cmd_str = cmd
            .get_args()
            .fold(cmd.get_program().to_owned(), |mut cmd, arg| {
                cmd.extend([" ".as_ref(), arg]);
                cmd
            });

        eprintln!("Running `{}`", cmd_str.to_string_lossy());

        let status = cmd.status()?;
        eprintln!("Status: {status}");
        let output = fs::read(out)?;
        let expected = fs::read(expected_out)?;
        if output == expected {
            eprintln!("Test passed");
        } else {
            failed = true;
            eprintln!("Test failed!");
            let output_str = String::from_utf8_lossy(&output);
            let expected_str = String::from_utf8_lossy(&expected);
            let diff = SimpleDiff::from_str(&output_str, &expected_str, "actual", "expected");
            eprintln!("{diff}");
        }
    }

    if failed {
        Err(DiffErr)?
    } else {
        Ok(())
    }
}
