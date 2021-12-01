#!/bin/bash

isyear() {
	case $1 in
		[[:digit:]][[:digit:]][[:digit:]][[:digit:]])
			;;
		*)
			return 1
			;;
	esac
}

check_pass() {
	e=

	# ignore cid
	for f in byr iyr eyr hgt hcl ecl pid
	do
		if [[ "$1" != *"${f}:"* ]]; then
			return 1
		fi
	done

	f=

	# validate each
	for v in $1
	do
		e=${v#*:}
		case "$v" in
			byr:*)
				isyear $e || return 1
				[ $e -lt 1920 -o $e -gt 2002 ] && return 1
				;;
			iyr:*)
				isyear $e || return 1
				[ $e -lt 2010 -o $e -gt 2020 ] && return 1
				;;
			eyr:*)
				isyear $e || return 1
				[ $e -lt 2020 -o $e -gt 2030 ] && return 1
				;;
			hgt:*)
				case $e in
					*cm)
						m=${e%cm}
						[ $m -lt 150 -o $m -gt 193 ] && return 1
						;;
					*in)
						m=${e%in}
						[ $m -lt 59 -o $m -gt 76 ] && return 1
						;;
					*)
						return 1
						;;
				esac
				;;
			hcl:*)
				case $e in
					\#[[:xdigit:]][[:xdigit:]][[:xdigit:]][[:xdigit:]][[:xdigit:]][[:xdigit:]])
						;;
					*)
						return 1
						;;
				esac
				;;
			ecl:*)
				case $e in
					amb|blu|brn|gry|grn|hzl|oth)
						;;
					*)
						return 1
						;;
				esac
				;;
			pid:*)
				case $e in
					[[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]][[:digit:]])
						;;
					*)
						return 1
						;;
				esac
				;;
		esac
	done

	return 0
}

total_count=0
invalid_count=0

total=
while read -r input
do
	if [ -n "$input" ]; then
		total+=" $input"
	else
		if ! check_pass "$total"; then
			invalid_count=$(($invalid_count + 1))
			[ "$f" ] && echo missing $f $total
			[ "$e" ] && echo bad value $e - $total
		fi
		total=
		total_count=$(($total_count + 1))
	fi
done < data

# count trailing data
total_count=$(($total_count + 1))
if ! check_pass "$total"; then
	invalid_count=$(($invalid_count + 1))
	[ "$e" ] && echo bad value $e - $total
fi

echo "total: $total_count"
echo "inval: $invalid_count"
echo "  val: $(($total_count - $invalid_count))"
