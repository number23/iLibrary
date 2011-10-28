CC = cc
CP = cp -rf
LN = ln -sf

GLIB = $$(pkg-config --cflags --libs glib-2.0)
CFLAGS = -Wall -W $(GLIB)

all:
	#@echo 'compile...'
	$(CC) main.c $(CFLAGS) -o smtpClient

install: all
	mv smtpClient $$HOME/bin/smtpClient
	#ln -sf $$HOME/bin/smtpClient $$HOME/.gnome2/nautilus-scripts/smtpClient
