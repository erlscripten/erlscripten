#!/bin/sh

cat $1 | sed -E "s/(erlps__.*__[0-9]+ = )function/\1function\*/g" | sed -E "s/(erlps__.*__[0-9]+\()/yield\* \1/g" | sed -E "s/function (.tco_loop)/function\* \1/g" | sed -E "s/= (.tco_loop\()/= yield\* \1/g" > /tmp/a
cp /tmp/a $1

