#!/bin/bash
# Cale Bierman
# PLC
# 9/23/14

# Enter your regular expression in the following line
REGEXP="(\bwork\b)?.*([$][0-9]+\.?[0-9]{0,2}){1}.*(hour|hr)"

usage ()
{
    cat <<EOF 
Usage: `basename $0` [OPTIONS] FILE
Classify FILE as a job advert or other piece of text, output "Found" or "Not found".

-v  Output matching lines 
-h  Display this help and exit
EOF
}

# Default arguments to egrep
ARGS="-q -l"

# Parse all command-line arguments 
while :
  do
  case $1 in

      -h | --help) # Display usage message
          usage
          exit 0
          ;;
      
      -v | --verbose) # Output matching lines
          ARGS=""
	  shift
          ;;
      
      --) # End of all options
          shift
          break
          ;;

      -*) # Unknown option
          usage 
          exit 2

          ;;
      *)  # no more options. Stop while loop
          break
          ;;
    esac
done

# Grep for expression with case-insensitive matching 
egrep $ARGS -i "$REGEXP" $1

# Evaluate exit code
case $? in
    0) echo "Found";;
    1) echo "Not Found";;
    *) echo "Error";;
esac
