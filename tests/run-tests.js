#!/usr/bin/env node

const path = require("path");
const fs = require("fs/promises");
const { execFileSync } = require("child_process");

const RED = "\x1b[0;31m";
const GREEN = "\x1b[0;32m";
const YELLOW = "\x1b[1;33m";
const BLUE = "\x1b[0;34m";
const NC = "\x1b[0m";

const ROOT_DIR = path.join(__dirname, "..");
const TESTS_DIR = path.join(ROOT_DIR, "tests");
const LOX_BINARY = path.join(ROOT_DIR, "lox");

const EX_SOFTWARE = 70;

async function fileExists(filePath) {
  try {
    await fs.access(filePath, fs.constants.F_OK);
    return true;
  } catch {
    return false;
  }
}

async function runTest(testFile) {
  const filename = path.basename(testFile, ".lox");
  const expectedFile = path.join(TESTS_DIR, `${filename}.expected`);

  if (!(await fileExists(expectedFile))) {
    return {
      filename,
      status: "skip",
      reason: `Expected file ${filename}.expected not found`,
    };
  }

  const expectedContent = await fs.readFile(expectedFile, "utf8");
  const expectedLines = expectedContent
    .split("\n")
    .filter((line) => line.length);

  let stdout = "";
  let stderr = "";
  let exitCode = 0;

  try {
    stdout = execFileSync(LOX_BINARY, [testFile], {
      stdio: "pipe",
      encoding: "utf8",
      maxBuffer: 10 * 1024 * 1024,
    });
  } catch (err) {
    exitCode = err.status || 1;
    stdout = err.stdout || "";
    stderr = err.stderr || "";
  }

  const actualLines = stdout.split("\n").filter((line) => line.length);

  const isExpectedError =
    expectedLines.length === 0 && exitCode === EX_SOFTWARE;
  if (isExpectedError) {
    return {
      filename,
      status: "pass",
      reason: "- Expected error (exit 70)",
    };
  }

  if (exitCode !== 0 && exitCode !== EX_SOFTWARE) {
    return {
      filename,
      status: "fail",
      reason: `Unexpected exit code ${exitCode}`,
      stderr: stderr.trim(),
      expected: expectedLines,
      actual: actualLines,
    };
  }

  const matches = [];
  const mismatches = [];
  const maxLines = Math.max(expectedLines.length, actualLines.length);

  for (let i = 0; i < maxLines; i++) {
    const expected = expectedLines[i];
    const actual = actualLines[i];

    if (expected === undefined) {
      mismatches.push({
        line: i,
        expected: "(missing)",
        actual: actual || "(empty)",
      });
    } else if (actual === undefined) {
      mismatches.push({
        line: i,
        expected: expected || "(empty)",
        actual: "(missing)",
      });
    } else if (expected === actual) {
      matches.push({ line: i, value: expected });
    } else {
      mismatches.push({ line: i, expected, actual });
    }
  }

  if (mismatches.length === 0) {
    return { filename, status: "pass", matches: matches.length };
  } else {
    return {
      filename,
      status: "fail",
      matches,
      mismatches,
      stderr: stderr.trim(),
    };
  }
}

function printResults(result) {
  if (result.status === "pass") {
    console.log(`${GREEN}✓${NC} ${result.filename}`);
    if (result.reason) {
      console.log(`  ${BLUE}${result.reason}${NC}`);
    }
  } else if (result.status === "skip") {
    console.log(`${YELLOW}⊘${NC} ${result.filename}`);
    console.log(`  ${YELLOW}${result.reason}${NC}`);
  } else if (result.status === "fail") {
    console.log(`${RED}✖${NC} ${result.filename}`);

    if (result.reason) {
      console.log(`  ${RED}${result.reason}${NC}`);
    }

    if (result.stderr) {
      console.log(`  ${RED}stderr:${NC} ${result.stderr}`);
    }

    if (result.matches) {
      for (const match of result.matches) {
        console.log(`  ${GREEN}✓${NC} ${match.value}`);
      }
    }

    if (result.mismatches) {
      for (const mismatch of result.mismatches) {
        console.log(
          `  ${RED}✖ ${YELLOW}${mismatch.expected}${NC} === ${RED}${mismatch.actual}${NC}\n`
        );
      }
    }
  }
}

async function main() {
  if (!(await fileExists(LOX_BINARY))) {
    console.log(`${RED}Error: lox binary not found at ${LOX_BINARY}${NC}`);
    console.log("Please run 'make' first to build the interpreter.");
    process.exit(1);
  }

  if (!(await fileExists(TESTS_DIR))) {
    console.log(`${RED}Error: tests directory not found at ${TESTS_DIR}${NC}`);
    process.exit(1);
  }

  console.log(`${BLUE}Running Lox tests...${NC}\n`);

  const results = {
    passed: 0,
    failed: 0,
    skipped: 0,
  };

  const testFiles = [];
  for await (const entry of fs.glob(path.join(TESTS_DIR, "*.lox"))) {
    testFiles.push(entry);
  }

  if (testFiles.length === 0) {
    console.log(`${YELLOW}No test files found in ${TESTS_DIR}${NC}`);
    process.exit(0);
  }

  testFiles.sort();

  for (const testFile of testFiles) {
    const result = await runTest(testFile);
    printResults(result);

    if (result.status === "pass") results.passed++;
    else if (result.status === "fail") results.failed++;
    else if (result.status === "skip") results.skipped++;
  }

  console.log(`${"=".repeat(50)}`);
  console.log(
    `${GREEN}Passed: ${results.passed}${NC} | ` +
      `${RED}Failed: ${results.failed}${NC} | ` +
      `${YELLOW}Skipped: ${results.skipped}${NC}`
  );
  console.log(
    `Total: ${results.passed + results.failed + results.skipped} tests`
  );

  process.exit(results.failed > 0 ? 1 : 0);
}

main().catch((err) => {
  console.error(`${RED}Fatal error:${NC}`, err);
  process.exit(1);
});
