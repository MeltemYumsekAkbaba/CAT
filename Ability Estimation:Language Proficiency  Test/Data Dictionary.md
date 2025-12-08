The data comes from an English proficiency assessment that is computer adaptive. It has three subsections measuring three subskills. Because it is computer adaptive test, as with other CATs the data is sparse. Some items are taken by few TTs. What is unique is the TTs are allowed to take multiple times saved in attempt number variable. 

### Input Data Structure

Although the dataset is not included, the structure is:

| Variable | Description |
|---------|-------------|
| `tt_id` | Test-taker identifier |
| `item_id` | Item identifier |
| `difficulty` | Pool-calibrated b-parameter |
| `skill` | Skill tested (Reading / Listening / Vocabulary) |
| `section` | Subsection of the CAT (Introduction / Reading / Vocabulary / Listening) |
| `attempt` | Attempt index (1, 2, 3, …) |
| `response` | Binary scored response |
| `response_time` | Date and time for the response |




