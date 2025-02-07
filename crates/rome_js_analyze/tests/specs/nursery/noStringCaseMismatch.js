// valid
s.toUpperCase() === "ABC";
s.toLowerCase() === "abc";
s.toUpperCase() === "20";
s.toLowerCase() === "20";
s.toLowerCase() === `eFg${12}`;
s.toLowerCase() == `eFg${12}`;

// invalid
const a = s.toUpperCase() === "abc";
const c = s.toUpperCase() == "abc";
const a2 = "abc" === s.toUpperCase();
if (s.toUpperCase() === "abc" && c == d && e == f) {}
while (s.toUpperCase() === "abc" && c == d && e == f) {}
while (s.toUpperCase() === "abc") {}
let b = s.toLowerCase() === `eFg`;
do {} while (s.toLowerCase() === "ABC");
for (; s.toLowerCase() === "ABC"; ) {}

switch (s.toUpperCase()) {
    case "ABC":
    case "abc":
    case "aBc":
    default:
}

for (; s["toLowerCase"]() === "ABC"; ) {}
for (; s[`toUpperCase`]() === "abc"; ) {}

switch (s["toLowerCase"]()) {
    case "Abc":
    case "aBc":
    case "abC":
    default:
}