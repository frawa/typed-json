#!/bin/bash

curl https://json-schema.org/draft/2020-12/schema -o schema.json
curl https://json-schema.org/draft/2020-12/meta/core -o meta/core.json
curl https://json-schema.org/draft/2020-12/meta/applicator -o meta/applicator.json
curl https://json-schema.org/draft/2020-12/meta/validation -o meta/validation.json
curl https://json-schema.org/draft/2020-12/meta/unevaluated -o meta/unevaluated.json
curl https://json-schema.org/draft/2020-12/meta/meta-data -o meta/meta-data.json
curl https://json-schema.org/draft/2020-12/meta/format-annotation -o meta/format-annotation.json
curl https://json-schema.org/draft/2020-12/meta/format-assertion -o meta/format-assertion.json
curl https://json-schema.org/draft/2020-12/meta/content -o meta/content.json

mkdir -p output
curl https://json-schema.org/draft/2020-12/output/schema -o output/schema.json
