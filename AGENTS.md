---

# Projection notice

Appended by `ws publish` from the research foundry workspace. **Do not edit this section.**

This repository is a projection. The canonical source is the workspace entity `synthyra_wolfram_paclet`
at `projects/serving/synthyra_wolfram_paclet/README.md`.

## If you change code here

Changes made in this clone do not reach the workspace on their own. Report what you changed
and at which commit so it can be brought back with `ws import Synthyra/synthyra-wolfram-paclet <sha>`. Do not
assume a later publish preserves your change: publish rebuilds this tree from the workspace
and refuses when GitHub HEAD is not the SHA it last recorded.

## Vendored code

`none` is vendored here from the workspace so this clone runs standalone. Do not edit a
vendored directory; edit it in the workspace and republish.

## Results

Log runs to Weights and Biases with the group set to the experiment id. That is how results
reach the workspace, through `ws capture`. Do not copy result files by hand.
