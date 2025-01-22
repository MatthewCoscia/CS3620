# SQL DSL Implementation in Racket

## Overview

This project is a domain-specific language (DSL) for querying and manipulating data, implemented with two approaches:
1. **core-lists.rkt**: Uses lists to store and process query results.
2. **core-streams.rkt**: Uses streams for improved performance in specific cases.

### Features

#### Core Features
1. **from**: Creates a `QueryResult` from a table, with optional column name prefixing.
2. **where**: Filters rows using a predicate.
3. **select**: Projects specified columns, raising errors for missing ones.
4. **join**: Nested-loop join for combining two datasets, checking for column conflicts.
5. **join/hash**: Hash-based join for faster performance in some scenarios.
6. **limit**: Keeps only the first `n` rows.
7. **distinct**: Removes duplicate rows.
8. **aggregate**: Groups and computes results based on specified columns.
9. **order-by**: Sorts rows by a column and comparator.
10. **extend**: Adds a computed column.

#### Extra Features
- **order-by**: Provides SQL-style sorting.
- **extend**: Dynamically computes and adds new columns.

## Avoiding Full Dataset Processing

### Operations That Avoid Full Processing
1. **limit**: Stops processing after retrieving `n` rows.
2. **where**: Short-circuits when used with `limit`.
3. **join/hash**: Lazily builds the hash table for efficient processing.

### Operations Requiring Full Processing
1. **distinct**: Scans the entire dataset to ensure uniqueness.
2. **aggregate**: Processes all rows to compute grouped results.
3. **order-by**: Sorts all rows, requiring the entire dataset.

### Explanation
Streams excel in cases where partial results are sufficient by delaying computations. However, full-dataset operations like sorting and aggregation inherently need to process all data.

## Performance Analysis: Join Methods

### Nested Loop Join (`join`)
- Iterates over both datasets in a nested way.
- Complexity: **O(m × n)** (m and n are dataset sizes).
- Time-intensive for large datasets.

### Hash Join (`join/hash`)
- Builds and probes a hash table for efficient lookups.
- Complexity: **O(m + n)**.
- Much faster due to fewer comparisons and optimized memory use.

### Observations
- **core-streams**: `join/hash` is faster due to lazy evaluation and minimized memory overhead.
- **core-lists**: `join/hash` processes the dataset entirely but is still faster than nested-loop joins.

## Test Results

### Query Example
```racket
(query/rows
  (from routes)
  (join/hash (from airlines #:qualify 'airline)
             'airline-id 'airline.airline-id)
  (where (lambda (row) (equal? (hash-ref row 'airline.name) "American Airlines")))
  (select 'source-airport 'destination-airport)
  (limit 3))
```

### Performance Comparison
| Implementation       | Time (ms) | GC Time (ms) |
|---------------------------|-----------|---------|
| `core-lists: join`        | 24,343    | 2,500   |
| `core-lists: join/hash`   | 219       |   109   |
| `core-streams: join`      | 19,611    | 2,765   |
| `core-streams: join/hash` | 225       |    78   |

### Explanation
- Hash joins avoid redundant comparisons via efficient hash table lookups.
- Stream-based hash joins minimize memory usage and optimize computation.

## Conclusion
Streams and hash joins significantly enhance the DSL's performance for large datasets. Operations like `limit` and `where` benefit from streams, while `join/hash` excels in join-intensive tasks.
