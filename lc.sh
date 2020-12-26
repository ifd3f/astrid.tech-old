#!/bin/bash

find . -type f \( -name '*.tsx' -or -name '*.ts' -or -name '*.jsx' -or -name '*.js' -or -name '*.scss' -or -name '*.py' \) \
  -not -path "./astrid_tech_frontend/node_modules/*" -not -path "./content/*"  -not -path "./astrid_tech_frontend/.cache/*" | \
  xargs wc -l