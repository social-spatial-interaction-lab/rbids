# rbids: R Interface for BIDS Motion Data

## Overview

`rbids` is an R package for managing and accessing Brain Imaging Data Structure (BIDS)
motion data. It provides a simple interface to work with BIDS-formatted datasets, making
it easier to load, filter, and analyze motion data.

## Basic Usage

```r
library(rbids)

# Initialize a BIDS dataset
bids <- Bids$new("/path/to/bids/dataset")

# Load motion data
motion_data <- bids$load_motion()

# Load motion data with filters
filtered_motion <- bids$load_motion(task = "rest", subject = "sub-01")

# Load event logs
event_logs <- bids$load_logs(task = "rest")

# Load session data
session_data <- bids$load_session()
```

## Key Features

- **Easy BIDS Navigation**: Automatically indexes and provides access to BIDS-compliant datasets
- **Flexible Data Loading**: Load motion files, event logs, and session information with powerful filtering options
- **Intelligent Type Handling**: Automatic detection and resolution of data type conflicts across files
- **Dataset Summary**: Quick overview of dataset contents including subjects, sessions, tasks, and datatypes

## Detailed Documentation

### Creating a BIDS Object

```r
# Create a BIDS object with read-only access (default)
bids <- Bids$new("/path/to/bids/dataset")

```

### Loading Data

```r
# Load all motion data
motion_data <- bids$load_motion()

# Filter by subject, task, session, etc.
filtered_data <- bids$load_motion(
  subject = "sub-01",
  task = "rest",
  session = "01"
)

# Load participant information
participants <- bids$load_participant()

# Load event logs
events <- bids$load_logs(task = "rest")

# Load session data
sessions <- bids$load_session()
```

### Listing Files

```r
# List motion files
motion_files <- bids$list_motion()

# List motion files with filters
filtered_files <- bids$list_motion(subject = "sub-01", task = "rest")

# List log files
log_files <- bids$list_logs()

# List session files
session_files <- bids$list_session()
```

### Loading Custom Files

```r
# Load specific files by path
files <- bids$load_files(c("/path/to/file1.tsv", "/path/to/file2.tsv"))
```

## Data Type Conflict Handling

The package intelligently handles data type conflicts between files with two modes:

- **strict mode** (default): Aborts loading when data type conflicts are detected
- **auto mode**: Automatically resolves conflicts by converting to appropriate types

In auto mode, numeric conversion is attempted first if possible (all values can be
converted), otherwise character conversion is used.

```r
# Auto-resolve type conflicts
data <- bids$load_motion(type_check = "auto")
```
