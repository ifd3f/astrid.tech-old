#!/bin/bash

find . -type f \( -name '*.tsx' -or -name '*.ts' -or -name '*.jsx' -or -name '*.js' -or -name '*.scss' \) \
  -not -path "./node_modules/*" -not -path "./content/*"  -not -path "./.cache/*" | \
  xargs wc -l