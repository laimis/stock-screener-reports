# SQL Connection Management Improvements

## Date: October 9, 2025

## Issues Found and Fixed

### 1. ✅ Fixed: `saveIndustrySequenceWithPoints` N+1 Connection Problem

**Before:**
- Created 1 connection to upsert sequence
- Created 1 connection to delete old points  
- Created N connections (one per point) to insert new points

**After:**
- Creates 1 connection to upsert sequence
- Uses a single transaction with batch operations for delete + all inserts
- Reduces from N+3 connections to 2 connections

**Impact:** For a sequence with 100 points, this reduces from 103 connections to 2 connections.

---

### 2. ✅ Fixed: `getIndustrySequencesWithQuery` N+1 Query Problem

**Before:**
- Created 1 connection to fetch sequences
- Created N connections (one per sequence) to fetch points for each sequence

**After:**
- Creates 1 connection to fetch all sequences
- Creates 1 connection to fetch ALL points for ALL sequences using `ANY(@sequenceids)`
- Groups points by sequence ID in memory

**Impact:** For 50 sequences, this reduces from 51 connections to 2 connections.

**Key Changes:**
- Modified `pointMapper` to also return the `sequenceid`
- Uses PostgreSQL's `ANY()` operator for efficient batch fetching
- Groups results in memory using F# `Map`
- Added early return for empty sequences

---

### 3. ✅ Documented: `saveScreenerResults` Connection Pattern

**Current State:**
- Still creates multiple connections (one per stock upsert, one per result insert)
- Added comment explaining the limitation

**Why Not Fixed:**
- `saveStock` uses `RETURNING *` to get the auto-generated stock ID
- We need the stock ID before we can insert the screener result
- PostgreSQL doesn't support batch `RETURNING` clauses in a way that maps back to input rows easily

**Future Optimization Path:**
If this becomes a bottleneck, consider:
1. Use `UNNEST()` with CTEs to batch insert stocks and return IDs with correlation
2. Or restructure to fetch existing stocks first, separate new vs existing, batch operations accordingly

---

## Additional Recommendations

### Connection Pooling
Verify your connection string includes pooling settings:
```
Host=...;Database=...;Username=...;Password=...;Maximum Pool Size=100;Connection Lifetime=0
```

### Monitor These Patterns

While not N+1 problems, these functions create many sequential connections:

1. **Heavy Transaction Functions:**
   - `deleteStock` - Uses transaction (good!)
   - `deleteScreener` - Uses transaction (good!)
   - `saveAlert` - Uses transaction (good!)

2. **Functions That Could Benefit from Batching:**
   - `saveScreenerResults` - As discussed above
   - `updateIndustryTrend` - If called in a loop, consider batch insert

### General Connection Best Practices

✅ **Good patterns you're already using:**
- Using `Sql.executeTransaction` for related operations
- Using `ON CONFLICT` for upserts instead of select-then-insert
- Parameterized queries throughout

⚠️ **Things to watch:**
- Any function called in a loop should be audited for connection reuse opportunities
- Consider adding explicit connection disposal in error cases if Npgsql.FSharp doesn't handle it

---

## Testing Recommendations

After these changes, you should:

1. **Run your existing test suite** to ensure behavior hasn't changed
2. **Monitor connection pool metrics** in production:
   - Connection pool size
   - Connection wait time
   - Connection creation rate
3. **Performance test** the modified functions with realistic data volumes

---

## Npgsql.FSharp Connection Behavior

The library typically handles connection disposal through the pipeline, but it's worth verifying:
- Connections created with `Sql.connect` should be disposed after `Sql.execute*` completes
- The library uses `use` bindings internally for proper disposal
- Connection pooling is managed by Npgsql itself, not the F# wrapper

---

## Summary

**Connections Saved Per Operation:**
- `saveIndustrySequenceWithPoints(100 points)`: 101 → 2 (98% reduction)
- `getIndustrySequencesWithQuery(50 sequences)`: 51 → 2 (96% reduction)

These changes will significantly reduce database connection overhead, especially under load or when processing large datasets.
