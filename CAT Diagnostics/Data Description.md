The data comes from an English proficiency assessment that is computer adaptive. It has three subsections measuring three subskills. TTs are allowed to take multiple times and there are multiple test attempts for each TT. 
For this project there are two data files used for analysis. The first one is the long data file:

### Long Data Structure

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

This long data is used to estimate TT overall and skill-specific θ. Please see "Ability Estimation: Language Proficiency Assessment" project in the repo.
 θ estimates are saved to wide data file that will be also used for this project.

### Wide Data Structure

| Variable | Description |
|---------|-------------|
| `session_id` | Identifier to differentiate session per TT (tt_id + attempt number) |
| `tt_id` | Test-taker identifier |
| `attempt` | Attempt index (1, 2, 3, …) |
| `I_itemID` | Binary scored response for each item, so there are as many I_itemID columns as the # of items |
| `F1` | Overall θ |
| `SE_F1` | Error of Overall θ |
| `Listening_F1` | Listening θ |
| `Listening_SE_F1` | Error of Listening θ |
| `Reading_F1` | Reading θ |
| `Reading_SE_F1` | Error of Reading θ |
| `Grammar_F1` | Grammar θ |
| `Grammar_SE_F1` | Error of Grammar θ |
