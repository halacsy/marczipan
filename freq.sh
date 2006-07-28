#!/bin/bash
awk '
BEGIN { FS="\t"}
{
	freq[$1]++ 

}
END {
	for (w in freq) print w "\t" freq[w]
}
'
