# Vocal interactions timing analysis

**\[experimental\]**

Carries out the analysis described in Demartsev et al. 2024 (doi:
10.1098/rstb.2023.0188) \#It works as follows: Make a table of all calls
within a given call category (set of calls), identify the one who gave
the call as the 'caller' For each call, identify all other individuals
('responders') who were present (calls labeled) on that day Also
identify the distance between the caller and responder at the time when
the call was given by the caller (if desired) Save this table as
callresp Note: This table DOES include self-responses (which can be
useful for looking at self-response patterns, or can be filtered out
later) Then for each of row in this table, look at the call sequence of
the 'responder' over time, lined up so that t = 0 = time of the 'caller'
call Run a kernel over this time series to smooth it with a bandwidth bw
= .1 sec (or as specified by parameters bw) Save the output to a matrix
as callresp_seqs, such that the indices in the table match the indices
in the matrix In subsequent code, these sequences can be further
explored and plotted. For instance, we can get the mean across all (or a
subset) of rows of the matrix for different conditions: Different
distance bins (aggregate only data from caller-responder pairs within a
range of distances from one another) Different age/sex/dominance classes
(aggregate only data from caller-responder pairs where the caller was a
certain status, and the responder was a certain status) Investigate
self-interactions Etc. callresp_table: data frame of call-response pairs
w/ relevant columns caller: [str](https://rdrr.io/r/utils/str.html) id
of the calling individual whose call triggers the sequence (lined up at
t = 0) responder: [str](https://rdrr.io/r/utils/str.html) id of the
responding individual whose subsequent calls are being monitored t0:
[numeric](https://rdrr.io/r/base/numeric.html) time of the caller call
in POSIXct form tf: [numeric](https://rdrr.io/r/base/numeric.html) time
of the caller call in POSIXct form distance:
[numeric](https://rdrr.io/r/base/numeric.html) distance between caller
and responder at the time of the original call a few other columns used
in processing callresp.seqs:
[matrix](https://rdrr.io/r/base/matrix.html) of call-response sequences,
where each row represents the sequence associated with each row in the
callresp data frame and each column represents a point in time tseq:
[vector](https://rdrr.io/r/base/vector.html) giving the time point
associated with the columns of callresp_seqs (in seconds)

## Usage

``` r
get_vocal_interactions_timing(
  calls,
  caller_calltype,
  responder_calltype,
  bw = 0.1,
  step = 0.02,
  max_lag = 3,
  trigger_on = "begin",
  xs = NULL,
  ys = NULL,
  timestamps = NULL,
  audioOn = NULL
)
```

## Arguments

- calls:

  data frame of calls, with columns call_type (call type, character
  string), t0 (start time in POSIXct UTC), tf (end time in POSIXct UTC),
  ind_idx (individual index, matching to rows in xs and ys matrices if
  present), and optionally time_idx (index of closest time point that
  matches to timestamps vector)

- bw:

  bandwidth of the kernel to use in the analysis

- step:

  time resolution (in sec)

- max_lag:

  max time lag to consider (in sec)

- trigger_on:

  string specifying if you should trigger on the beginning of calls
  ('begin') or end of calls ('end')

- xs:

  `N x n_times` matrix of eastings for all individuals

- ys:

  `N x n_times` matrix of northings for all individuals

- timestamps:

  vector of POSIXct (UTC) timestamps corresponding to columns in xs and
  ys

- audioOn:

  `N x n_times` matrix of whether each individual has audio data at each
  second (T or F)

- caller_calltypes:

  string giving a regular expression for what initiator (focal) call
  types to include

- responder_calltypes:

  string giving a regular expression for what repsonder call types to
  include

## Value

Returns a list containing: `callresp`: the table of callers, responders,
call times, distances, etc. `callresp_seqs`: vocal sequence of the
responder relative to the time of the caller's call `out$tseq`: time
sequence used (maps to the columns of callresp_seqs)
`out$callresp_means_all`: mean across the columns of callresp_seqs - the
average response to a call (across all data, including self responses)
`out$callresp_means_other`: mean across the columns of callresp_seqs -
the average response to a call including only non-self responses, i.e.
responses to others individuals) `out$callresp_means_self`: mean across
the columnns of callresp_seqs - the average response to a call including
only self-responses), i.e. the typical time between calls in a single
individual's vocal sequence

## Author

Ariana Strandburg-Peshkin (primary author)

NOT YET CODE REVIEWED
