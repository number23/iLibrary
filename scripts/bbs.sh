#!/usr/bin/expect
set timeout 580
set password my-password

spawn ssh -1 N23@127.0.0.1 -p 8022
#spawn ssh N23@60.2.251.8
#spawn ssh N23@222.222.192.11
#spawn ssh -1 N23@bbs.newsmth.net

expect {
    "(yes/no)?" {
        send "yes\n"
        expect "assword:"
        send "$pasword\n"
    }

    "assword:" {
        send "$password\n"
    }
}

interact {
	"\033OH" {send "\033\[1~"}
	"\033OF" {send "\033\[4~"}
	timeout 580 {send " "}
}
