I have a Cecelia project called "{project_name}" (uid {project_uid}) with {n_images} time-lapse images. Please, for every image:

1. segment the cells and measure them (`segment.cellposeMeasure`) into a NEW value name — keep any existing value names as they are;
2. track them (`tracking.bayesian_tracking`) and compute track measures (`tracking.track_measures`);
3. then fit two behaviour states across all images with `behaviour.hmm_states` on the tracked cells.

Run the tasks through Cecelia (the REPL `run_task` / `run_tasks`), not by reimplementing them. I'm away until tomorrow, so work it out yourself — don't wait for me.

When you are done, end your final message with exactly one line of the form

RESULT {{"valueName": "<the label set your tracks and behaviours are in>", "stateColumn": "<the obs column holding the behaviour state>"}}
