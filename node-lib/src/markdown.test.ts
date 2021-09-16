import { getMarkdownExcerpt, renderMarkdown } from ".";

const SMOKE_TEST_SOURCE = `
# Here's a heading

This is a *hello world* command in the **C Programming Language**:

\`\`\`c
#include <stdio.h>

int main() {
    printf("Hello world!");
    return 0;
}
\`\`\`
`;

describe("renderMarkdown", function () {
  it("passes the smoke test", async function () {
    await renderMarkdown(SMOKE_TEST_SOURCE, "/tmp");
  });
});

describe("getMarkdownExcerpt", function () {
  it("passes the smoke test", async function () {
    await getMarkdownExcerpt(SMOKE_TEST_SOURCE, 53);
  });
});
