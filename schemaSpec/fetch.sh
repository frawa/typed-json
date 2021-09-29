#!/bin/bash

curl https://json-schema.org/draft/2020-12/schema -o schema.json
curl https://json-schema.org/draft/2020-12/meta/core -o core.json
curl https://json-schema.org/draft/2020-12/meta/applicator -o applicator.json
curl https://json-schema.org/draft/2020-12/meta/validation -o validation.json
curl https://json-schema.org/draft/2020-12/meta/unevaluated -o unevaluated.json
curl https://json-schema.org/draft/2020-12/meta/meta-data -o meta-data.json
curl https://json-schema.org/draft/2020-12/meta/format-annotation -o format-annotation.json
curl https://json-schema.org/draft/2020-12/meta/content -o content.json

mkdir -p output
curl https://json-schema.org/draft/2020-12/output/schema -o output/schema.json
