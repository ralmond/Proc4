#!/bin/bash
query="{timestamp: {\"\$gt\": {\"\$date\": \""
query+=$1
query+="\"}}, active:true, voided:false}"
echo $query
mongoexport -d gameLRS -c statements -q $query -f statement,timestamp --jsonArray
