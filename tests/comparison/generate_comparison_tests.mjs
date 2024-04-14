import { writeFileSync } from "fs";

for (let i = -5; i <= 5; i++) {
  for (let j = -5; j <= 5; j++) {
    save(i, j, "==", "eq", i === j);
    save(i, j, "!=", "neq", i !== j);
    save(i, j, "<", "ls", i < j);
    save(i, j, "<=", "lse", i <= j);
    save(i, j, ">", "gr", i > j);
    save(i, j, ">=", "gr", i >= j);
  }
}

function save(i, j, op, opname, result) {
  const contents = `// Int | ${result ? 1 : 0}
${i} ${op} ${j}
`;
  writeFileSync(`tests/comparison/compare_${i}_${j}_${opname}.brick`, contents);
}
