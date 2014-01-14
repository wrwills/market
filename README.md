
Instructions:

Running market simulator: sbt startScript
then target/start

The market simulation runs.  It fails on the last line of the csv file, but that's expected I imagine. Logging usually happens 
every 100ms but sometimes it can be off.  I can't at present see what I could do about that.  The logs happen asynchronously in
a separate actor so their won't be any delay from file io in the timestamps.  I don't have experience with trying to use akka 
scheduling at this fine grained a level.

The unoptimised version of CountCharacters has been implemented.

Queue has been implemented in the same way the scala library does it with 2 lists.  But I don't think there's any way to make 
head O(1) and dequeue O(1) amortised using this implementation.  My next step would be to try to implement it by backing it with a
finger tree.