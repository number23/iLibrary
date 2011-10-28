/*
 * Description:
 *   use SMTP(Simple Mail Transfer Protocol) to send mail 
 *     detail: http://en.wikipedia.org/wiki/Simple_Mail_Transfer_Protocol
 *
 * Requirements:
 *   glib-2.0
 *   the config file:
 *
 * vi $HOME/.LoginAccount.txt
 * [mailClient]
 * host = smtp.qq.com
 * port = 25
 * user = ***
 * pass = ***
 * fr = xxxxxx@qq.com
 * to = xxxxxx@gmail.com
 *
 * Compile: cc main.c -Wall -W $(pkg-config --cflags --libs glib-2.0) -o smtpClient
 * 
 * Usage:
 *   smtpClient [OPTION...] [FILE...]
 *
 * Help Options:
 * -?, --help                    Show help options
 *
 * Application Options:
 *   -e, --addr=ADDR               mail address
 *   -a, --atta                    attachment flag
 *   -s, --signature=SIGNATURE     the signature file add to mail
 *
 */

#include <glib.h>
#include <glib/gstdio.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <time.h>

#include <netdb.h>
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>

static const char *config_filename = ".LoginAccount.txt";
static const char *config_label = "mailClient";
static const char *end_flag = "\r\n";
static const char *email_flag = "===== Baby, I love you. ^-^ https://twitter.com/number23_cn =====";

const char *log_time();
int sendCmd(const int, const char*);
int fetchCmd(const int, const char *);
int connection(const char *, const int);
int login(const int, const char *, const char *, const char *);
int sendmail_head (const int, const char *, const char *, const char *);
void sendmail_file(const int, const char *);
int sendmail_tail(const int);
const char *get_sub_string(const char *, const gsize, const gsize);
void split_base64(const int, const char *, const gsize);
void sendmail_data(const int, const char *);
void sendmail_atta(const int, const char *);

static void error_func(GOptionContext *context,
                       GOptionGroup *group,
                       gpointer data,
                       GError **error) {
    if (*error && (*error)->message) {
        gchar *progname = g_get_prgname();
        g_print("%s: %s\nTry '%s --help' for more information.\n",
                progname,
                (*error)->message,
                progname);
        exit(1);
    }
}

int
main (int argc, char *argv[])
{
    gchar           *addr = NULL;
    gchar           *signature = NULL;
    gboolean        flag = FALSE;
    GOptionEntry    entries[] =
    {
        {"addr", 'e', 0, G_OPTION_ARG_FILENAME, &addr,
         "mail address", "ADDR"},
        { "atta", 'a', 0, G_OPTION_ARG_NONE, &flag,
          "attachment flag", NULL },
        {"signature", 's', 0, G_OPTION_ARG_FILENAME, &signature,
         "the signature file add to mail", "SIGNATURE"},
        {NULL}
    };
    GOptionContext *context;
    GError         *error = NULL;

    context = g_option_context_new("[FILE...]");
    g_option_context_add_main_entries(context, entries, NULL);
    g_option_context_set_help_enabled(context, TRUE);
    g_option_group_set_error_hook(g_option_context_get_main_group(context),
                                  (GOptionErrorFunc)error_func);
    g_option_context_parse(context, &argc, &argv, &error);

    if (error) {
        perror(error->message);
        return error->code;
    }
    g_option_context_free(context);

    if (argc == 1) g_error("No file to Send");
    g_print("check files:\n"); 
    gchar *config_path = g_build_filename(g_get_home_dir(), 
                                          config_filename, 
                                          NULL);
    if (g_access(config_path, R_OK) == 0) 
        g_print("\t%s: OK\n", config_path);
    else 
        g_error("\t%s: can't access\n", config_path);
    if (signature) {
        if (g_access(signature, R_OK) == 0) 
            g_print("\t%s: OK\n", signature);
        else 
            g_error("\t%s: can't access\n", signature);
    }
    int i = 0;
    for (i = 1; argv[i] != NULL; i++) {
        if (g_access(argv[i], R_OK) == 0) 
            g_print("\t%s: OK\n", argv[i]);
        else 
            g_error("\t%s: can't access\n", argv[i]);
    }

    GKeyFile *config;
    config = g_key_file_new();
    g_key_file_load_from_file(config, config_path, 0, NULL);
    gchar *host = g_key_file_get_string(config, config_label, "host", NULL);
    gint   port = g_key_file_get_integer(config, config_label, "port", NULL);
    gchar *user = g_key_file_get_string(config, config_label, "user", NULL);
    gchar *pass = g_key_file_get_string(config, config_label, "pass", NULL);
    gchar *fr = g_key_file_get_string(config, config_label, "fr", NULL);
    gchar *to = g_key_file_get_string(config, config_label, "to", NULL);

    const int fd = connection(host, port);
    if (addr) g_stpcpy(to, addr);
    if ((login(fd, host, user, pass)) == -1) g_error("login failed");
    for (argv++; *argv != NULL; argv++) {
        if (sendmail_head(fd, fr, to, *argv) == -1)
            g_error("sendmail_head failed");
        if (flag) {
            sendmail_atta(fd, *argv);
            if (signature) sendmail_file(fd, signature);
        } else {
            sendmail_file(fd, *argv);
            if (signature) sendmail_file(fd, signature);
        }
        if (sendmail_tail(fd) == -1)
            g_error("sendmail_tail failed");
    }
    close(fd);
    return 0;
}


int
sendCmd(const int fd, const char* s)
{
    int             bytes;
    g_print("%s%s%s%s%s",
             "Send: [",
             log_time(),
             "] ",
             "CMD=",
             s);
    if ((bytes = send(fd, s, strlen(s), 0)) == -1) {
        perror("send error");
        return -1;
    }

    return bytes;
}

int
fetchCmd(const int fd, const char *s)
{
    char            buff[1024];
    int             bytes;
    char            *tmp;

    if ((bytes = recv(fd, buff, 1024, 0)) == -1) {
        perror("recv error");
        return(-1);
    }
    buff[bytes] = '\0';
    g_print("%s%s%s%s%s",
             "Recv: [",
             log_time(),
             "] ",
             "Result=",
             buff);
    tmp = g_strndup(buff, 3);
    if (strcmp(tmp, s) != 0)
        return -1;
    else
        return 0;
}

const char *log_time()
{
    char           *ct;
    time_t          t;
    t = time(NULL);
    ct = ctime(&t);
    *(strchr(ct, '\n')) = '\0';
    return ct;
}

int
connection(const char *host, const int port)
{
    int             sockfd;
    struct sockaddr_in server_addr;
    struct hostent *server_ip;
    while ((server_ip = gethostbyname(host)) == NULL) {
        herror("gethostbyname error");
        sleep(30);
    }

    if ((sockfd = socket(AF_INET, SOCK_STREAM, 0)) == -1) {
        perror("socket error");
        return -1;
    }
    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(port);
    server_addr.sin_addr = *((struct in_addr *) server_ip->h_addr);
    bzero(&(server_addr.sin_zero), 8);
    if (connect(sockfd, (struct sockaddr *)
        & server_addr, sizeof(struct sockaddr)) == -1) {
        perror("connect error");
        return -1;
    }
    if (fetchCmd(sockfd, "220") == -1)
        return -1;

    return sockfd;
}

int
login(const int fd, const char *host, const char *user, const char *pass)
{
    GString *s = g_string_new("");
    g_string_append(s, "EHLO ");
    g_string_append(s, host);
    g_string_append(s, end_flag);
    sendCmd(fd, s->str);
    if (fetchCmd(fd, "250") == -1)
        return -1;

    g_string_erase(s, 0, -1);
    g_string_append(s, "AUTH LOGIN");
    g_string_append(s, end_flag);
    sendCmd(fd, s->str);
    if (fetchCmd(fd, "334") == -1)
        return -1;

    g_string_erase(s, 0, -1);
    g_string_append(s,  g_base64_encode((const guchar *)user, strlen(user)));
    g_string_append(s, end_flag);
    sendCmd(fd, s->str);
    if (fetchCmd(fd, "334") == -1)
        return -1;

    g_string_erase(s, 0, -1);
    g_string_append(s,  g_base64_encode((const guchar *)pass, strlen(pass)));
    g_string_append(s, end_flag);
    sendCmd(fd, s->str);
    if (fetchCmd(fd, "235") == -1)
        return -1;

    g_string_free(s, TRUE);
    return 0;
}

int sendmail_head (const int fd,
                   const char *fr,
                   const char *to,
                   const char *subject)
{
    GString *s = g_string_new("");
    g_string_append(s, "MAIL FROM:");
    g_string_append(s, "<");
    g_string_append(s, fr);
    g_string_append(s, ">");
    g_string_append(s, end_flag);
    sendCmd(fd, s->str);
    if (fetchCmd(fd, "250") == -1)
        return -1;

    g_string_erase(s, 0, -1);
    g_string_append(s, "RCPT TO:");
    g_string_append(s, "<");
    g_string_append(s, to);
    g_string_append(s, ">");
    g_string_append(s, end_flag);
    sendCmd(fd, s->str);
    if (fetchCmd(fd, "250") == -1)
        return -1;

    g_string_erase(s, 0, -1);
    g_string_append(s, "DATA");
    g_string_append(s, end_flag);
    sendCmd(fd, s->str);
    if (fetchCmd(fd, "354") == -1)
        return -1;

    g_string_erase(s, 0, -1);
    g_string_append(s, "From:");
    g_string_append(s, fr);
    g_string_append(s, end_flag);
    g_string_append(s, "To:");
    g_string_append(s, to);
    g_string_append(s, end_flag);

    g_string_append(s, "Subject:");
    g_string_append(s, subject);
    g_string_append(s, end_flag);
    g_string_append(s, "Mime-Version: 1.0");
    g_string_append(s, end_flag);
    sendCmd(fd, s->str);

    g_string_erase(s, 0, -1);
    g_string_append(s, "Content-Type: multipart/mixed;Boundary=\"");
    g_string_append(s, email_flag);
    g_string_append(s, "\"");
    g_string_append(s, end_flag);
    g_string_append(s, end_flag);
    sendCmd(fd, s->str);

    g_string_free(s, TRUE);
    return 0;
}

void
sendmail_file(const int fd, const char *file)
{
    GString *s = g_string_new("");
    g_string_append(s, "--");
    g_string_append(s, email_flag);
    g_string_append(s, end_flag);
    g_string_append(s, "Content-Type: text/plain; charset=\"utf-8\";");
    g_string_append(s, end_flag);
    g_string_append(s, "Content-Transfer-Encoding: base64");
    g_string_append(s, end_flag);
    g_string_append(s, end_flag);
    sendCmd(fd, s->str);
    g_string_free(s, TRUE);

    sendmail_data(fd, file);
}

void
sendmail_atta(const int fd, const char *file)
{
    GString *s = g_string_new("");
    g_string_append(s, "--");
    g_string_append(s, email_flag);
    g_string_append(s, end_flag);
    g_string_append(s, "Content-Type:application/octet-stream;name=\"");
    g_string_append(s, file);
    g_string_append(s, "\"");
    g_string_append(s, end_flag);
    g_string_append(s, "Content-Transfer-Encoding: base64");
    g_string_append(s, end_flag);
    g_string_append(s, "Content-Disposition:attachment;filename=\"");
    g_string_append(s, file);
    g_string_append(s, "\"");
    g_string_append(s, end_flag);
    g_string_append(s, end_flag);
    sendCmd(fd, s->str);
    g_string_free(s, TRUE);

    sendmail_data(fd, file);
}

int sendmail_tail(const int fd)
{
    GString *s = g_string_new("");
    g_string_append(s, end_flag);
    g_string_append(s, end_flag);
    g_string_append(s, "--");
    g_string_append(s, email_flag);
    g_string_append(s, "--");
    g_string_append(s, end_flag);
    sendCmd(fd, s->str);

    g_string_erase(s, 0, -1);
    g_string_append(s, end_flag);
    g_string_append(s, ".");
    g_string_append(s, end_flag);
    sendCmd(fd, s->str);
    if (fetchCmd(fd, "250") == -1)
        return -1;

    g_string_free(s, TRUE);
    return 0;
}

const char           *
get_sub_string(const char *str, const gsize start, const gsize end)
{
    gsize        n = end - start;
    static char     stbuf[256];
    if (start >= strlen(str)) {
        return NULL;
    } else {
        strncpy(stbuf, str + start, n);
        stbuf[n] = '\0';
        return stbuf;
    }
}

void
split_base64(const int fd, const char *text, const gsize len)
{
    gchar *code = g_base64_encode((const guchar *)text, len);
    GString *s = g_string_new("");
    guint start = 0, end = 0;
    int bytes = -1;
    gsize l = strlen(code);
    g_print("split_base64 ...start\n");
    for(end = 78; start < l; start +=78, end += 78) {
        g_string_append(s, get_sub_string(code, start, end));
        g_string_append(s, "\n");
        if ((bytes = send(fd, s->str, s->len, 0)) == -1)
            g_error("send error in split_base64");
        g_string_erase(s, 0, -1);
    }
    g_print("split_base64 ...done\n");
    g_free(code);
    g_string_free(s, TRUE);
}

void sendmail_data(const int fd, const char *file)
{
    GString *s = g_string_new("");
    g_string_append(s, end_flag);
    g_string_append(s, end_flag);
    sendCmd(fd, s->str);

    gchar *text;
    GError *err = NULL;
    gsize len;
    if (g_file_get_contents(file, &text, &len, &err))
        split_base64(fd, text, len);

    g_string_erase(s, 0, -1);
    g_string_append(s, end_flag);
    g_string_append(s, end_flag);
    sendCmd(fd, s->str);

    g_free(text);
    g_string_free(s, TRUE);
}
