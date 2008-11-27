#include "cpc_runtime.h"
typedef unsigned int size_t ;
typedef int wchar_t ;
typedef struct {
int  quot ;
int  rem ;
}
div_t ;
typedef struct {
long  int  quot ;
long  int  rem ;
}
ldiv_t ;
extern size_t (__ctype_get_mb_cur_max (void ));
extern double (atof (__const char *__nptr ));
extern int (atoi (__const char *__nptr ));
extern long int (atol (__const char *__nptr ));
extern long long int (atoll (__const char *__nptr ));
extern double (strtod (__const char *__restrict __nptr , char **__restrict __endptr ));
extern long int (strtol (__const char *__restrict __nptr , char **__restrict __endptr , int __base ));
extern unsigned long int (strtoul (__const char *__restrict __nptr , char **__restrict __endptr , int __base ));
extern long long int (strtoq (__const char *__restrict __nptr , char **__restrict __endptr , int __base ));
extern unsigned long long int (strtouq (__const char *__restrict __nptr , char **__restrict __endptr , int __base ));
extern long long int (strtoll (__const char *__restrict __nptr , char **__restrict __endptr , int __base ));
extern unsigned long long int (strtoull (__const char *__restrict __nptr , char **__restrict __endptr , int __base ));
extern __inline double (atof (__const char *__nptr ));
extern __inline double atof (__const char *__nptr ){
return strtod(__nptr, ((char **)((void *)0)));
}
extern __inline int (atoi (__const char *__nptr ));
extern __inline int atoi (__const char *__nptr ){
return ((int )strtol(__nptr, ((char **)((void *)0)), 10));
}
extern __inline long int (atol (__const char *__nptr ));
extern __inline long int atol (__const char *__nptr ){
return strtol(__nptr, ((char **)((void *)0)), 10);
}
extern __inline long long int (atoll (__const char *__nptr ));
extern __inline long long int atoll (__const char *__nptr ){
return strtoll(__nptr, ((char **)((void *)0)), 10);
}
extern char *(l64a (long int __n ));
extern long int (a64l (__const char *__s ));
typedef unsigned char __u_char ;
typedef unsigned short int __u_short ;
typedef unsigned int __u_int ;
typedef unsigned long int __u_long ;
typedef signed char __int8_t ;
typedef unsigned char __uint8_t ;
typedef signed short int __int16_t ;
typedef unsigned short int __uint16_t ;
typedef signed int __int32_t ;
typedef unsigned int __uint32_t ;
typedef signed long long int __int64_t ;
typedef unsigned long long int __uint64_t ;
typedef long long int __quad_t ;
typedef unsigned long long int __u_quad_t ;
typedef __u_quad_t __dev_t ;
typedef unsigned int __uid_t ;
typedef unsigned int __gid_t ;
typedef unsigned long int __ino_t ;
typedef __u_quad_t __ino64_t ;
typedef unsigned int __mode_t ;
typedef unsigned int __nlink_t ;
typedef long int __off_t ;
typedef __quad_t __off64_t ;
typedef int __pid_t ;
typedef struct {
int  (__val [2]);
}
__fsid_t ;
typedef long int __clock_t ;
typedef unsigned long int __rlim_t ;
typedef __u_quad_t __rlim64_t ;
typedef unsigned int __id_t ;
typedef long int __time_t ;
typedef unsigned int __useconds_t ;
typedef long int __suseconds_t ;
typedef int __daddr_t ;
typedef long int __swblk_t ;
typedef int __key_t ;
typedef int __clockid_t ;
typedef void *__timer_t ;
typedef long int __blksize_t ;
typedef long int __blkcnt_t ;
typedef __quad_t __blkcnt64_t ;
typedef unsigned long int __fsblkcnt_t ;
typedef __u_quad_t __fsblkcnt64_t ;
typedef unsigned long int __fsfilcnt_t ;
typedef __u_quad_t __fsfilcnt64_t ;
typedef int __ssize_t ;
typedef __off64_t __loff_t ;
typedef __quad_t *__qaddr_t ;
typedef char *__caddr_t ;
typedef int __intptr_t ;
typedef unsigned int __socklen_t ;
typedef __u_char u_char ;
typedef __u_short u_short ;
typedef __u_int u_int ;
typedef __u_long u_long ;
typedef __quad_t quad_t ;
typedef __u_quad_t u_quad_t ;
typedef __fsid_t fsid_t ;
typedef __loff_t loff_t ;
typedef __ino_t ino_t ;
typedef __dev_t dev_t ;
typedef __gid_t gid_t ;
typedef __mode_t mode_t ;
typedef __nlink_t nlink_t ;
typedef __uid_t uid_t ;
typedef __off_t off_t ;
typedef __pid_t pid_t ;
typedef __id_t id_t ;
typedef __ssize_t ssize_t ;
typedef __daddr_t daddr_t ;
typedef __caddr_t caddr_t ;
typedef __key_t key_t ;
typedef __time_t time_t ;
typedef __clockid_t clockid_t ;
typedef __timer_t timer_t ;
typedef unsigned long int ulong ;
typedef unsigned short int ushort ;
typedef unsigned int uint ;
typedef int int8_t ;
typedef int int16_t ;
typedef int int32_t ;
typedef int int64_t ;
typedef unsigned int u_int8_t ;
typedef unsigned int u_int16_t ;
typedef unsigned int u_int32_t ;
typedef unsigned int u_int64_t ;
typedef int register_t ;
typedef int __sig_atomic_t ;
typedef struct {
unsigned  long  int  (__val [(1024 / (8 * sizeof(unsigned long int )))]);
}
__sigset_t ;
typedef __sigset_t sigset_t ;
struct timespec {
__time_t  tv_sec ;
long  int  tv_nsec ;
}
;
struct timeval {
__time_t  tv_sec ;
__suseconds_t  tv_usec ;
}
;
typedef __suseconds_t suseconds_t ;
typedef long int __fd_mask ;
typedef struct {
__fd_mask  (__fds_bits [(1024 / (8 * sizeof(__fd_mask )))]);
}
fd_set ;
typedef __fd_mask fd_mask ;
extern int (select (int __nfds , fd_set *__restrict __readfds , fd_set *__restrict __writefds , fd_set *__restrict __exceptfds , struct timeval *__restrict __timeout ));
extern int (pselect (int __nfds , fd_set *__restrict __readfds , fd_set *__restrict __writefds , fd_set *__restrict __exceptfds , const struct timespec *__restrict __timeout , const __sigset_t *__restrict __sigmask ));
extern unsigned int (gnu_dev_major (unsigned long long int __dev ));
extern unsigned int (gnu_dev_minor (unsigned long long int __dev ));
extern unsigned long long int (gnu_dev_makedev (unsigned int __major , unsigned int __minor ));
extern __inline unsigned int (gnu_dev_major (unsigned long long int __dev ));
extern __inline unsigned int gnu_dev_major (unsigned long long int __dev ){
return (((__dev >> 8) & 4095) | (((unsigned int )(__dev >> 32)) & (~ 4095)));
}
extern __inline unsigned int (gnu_dev_minor (unsigned long long int __dev ));
extern __inline unsigned int gnu_dev_minor (unsigned long long int __dev ){
return ((__dev & 255) | (((unsigned int )(__dev >> 12)) & (~ 255)));
}
extern __inline unsigned long long int (gnu_dev_makedev (unsigned int __major , unsigned int __minor ));
extern __inline unsigned long long int gnu_dev_makedev (unsigned int __major , unsigned int __minor ){
return ((((__minor & 255) | ((__major & 4095) << 8)) | (((unsigned long long int )(__minor & (~ 255))) << 12)) | (((unsigned long long int )(__major & (~ 4095))) << 32));
}
typedef __blkcnt_t blkcnt_t ;
typedef __fsblkcnt_t fsblkcnt_t ;
typedef __fsfilcnt_t fsfilcnt_t ;
typedef unsigned long int pthread_t ;
typedef union {
char  (__size [36]);
long  int  __align ;
}
pthread_attr_t ;
typedef struct __pthread_internal_slist {
struct __pthread_internal_slist  *__next ;
}
__pthread_slist_t ;
typedef union {
struct __pthread_mutex_s {
int  __lock ;
unsigned  int  __count ;
int  __owner ;
int  __kind ;
unsigned  int  __nusers ;
union {
int  __spins ;
__pthread_slist_t  __list ;
}
 ;
}
 __data ;
char  (__size [24]);
long  int  __align ;
}
pthread_mutex_t ;
typedef union {
char  (__size [4]);
int  __align ;
}
pthread_mutexattr_t ;
typedef union {
struct {
int  __lock ;
unsigned  int  __futex ;
unsigned  long  long  int  __total_seq ;
unsigned  long  long  int  __wakeup_seq ;
unsigned  long  long  int  __woken_seq ;
void  *__mutex ;
unsigned  int  __nwaiters ;
unsigned  int  __broadcast_seq ;
}
 __data ;
char  (__size [48]);
long  long  int  __align ;
}
pthread_cond_t ;
typedef union {
char  (__size [4]);
int  __align ;
}
pthread_condattr_t ;
typedef unsigned int pthread_key_t ;
typedef int pthread_once_t ;
typedef union {
struct {
int  __lock ;
unsigned  int  __nr_readers ;
unsigned  int  __readers_wakeup ;
unsigned  int  __writer_wakeup ;
unsigned  int  __nr_readers_queued ;
unsigned  int  __nr_writers_queued ;
unsigned  char  __flags ;
unsigned  char  __shared ;
unsigned  char  __pad1 ;
unsigned  char  __pad2 ;
int  __writer ;
}
 __data ;
char  (__size [32]);
long  int  __align ;
}
pthread_rwlock_t ;
typedef union {
char  (__size [8]);
long  int  __align ;
}
pthread_rwlockattr_t ;
typedef volatile int pthread_spinlock_t ;
typedef union {
char  (__size [20]);
long  int  __align ;
}
pthread_barrier_t ;
typedef union {
char  (__size [4]);
int  __align ;
}
pthread_barrierattr_t ;
extern long int (random (void ));
extern void (srandom (unsigned int __seed ));
extern char *(initstate (unsigned int __seed , char *__statebuf , size_t __statelen ));
extern char *(setstate (char *__statebuf ));
struct random_data {
int32_t  *fptr ;
int32_t  *rptr ;
int32_t  *state ;
int  rand_type ;
int  rand_deg ;
int  rand_sep ;
int32_t  *end_ptr ;
}
;
extern int (random_r (struct random_data *__restrict __buf , int32_t *__restrict __result ));
extern int (srandom_r (unsigned int __seed , struct random_data *__buf ));
extern int (initstate_r (unsigned int __seed , char *__restrict __statebuf , size_t __statelen , struct random_data *__restrict __buf ));
extern int (setstate_r (char *__restrict __statebuf , struct random_data *__restrict __buf ));
extern int (rand (void ));
extern void (srand (unsigned int __seed ));
extern int (rand_r (unsigned int *__seed ));
extern double (drand48 (void ));
extern double (erand48 (unsigned short int (__xsubi [3])));
extern long int (lrand48 (void ));
extern long int (nrand48 (unsigned short int (__xsubi [3])));
extern long int (mrand48 (void ));
extern long int (jrand48 (unsigned short int (__xsubi [3])));
extern void (srand48 (long int __seedval ));
extern unsigned short int *(seed48 (unsigned short int (__seed16v [3])));
extern void (lcong48 (unsigned short int (__param [7])));
struct drand48_data {
unsigned  short  int  (__x [3]);
unsigned  short  int  (__old_x [3]);
unsigned  short  int  __c ;
unsigned  short  int  __init ;
unsigned  long  long  int  __a ;
}
;
extern int (drand48_r (struct drand48_data *__restrict __buffer , double *__restrict __result ));
extern int (erand48_r (unsigned short int (__xsubi [3]), struct drand48_data *__restrict __buffer , double *__restrict __result ));
extern int (lrand48_r (struct drand48_data *__restrict __buffer , long int *__restrict __result ));
extern int (nrand48_r (unsigned short int (__xsubi [3]), struct drand48_data *__restrict __buffer , long int *__restrict __result ));
extern int (mrand48_r (struct drand48_data *__restrict __buffer , long int *__restrict __result ));
extern int (jrand48_r (unsigned short int (__xsubi [3]), struct drand48_data *__restrict __buffer , long int *__restrict __result ));
extern int (srand48_r (long int __seedval , struct drand48_data *__buffer ));
extern int (seed48_r (unsigned short int (__seed16v [3]), struct drand48_data *__buffer ));
extern int (lcong48_r (unsigned short int (__param [7]), struct drand48_data *__buffer ));
extern void *(malloc (size_t __size ));
extern void *(calloc (size_t __nmemb , size_t __size ));
extern void *(realloc (void *__ptr , size_t __size ));
extern void (free (void *__ptr ));
extern void (cfree (void *__ptr ));
extern void *(alloca (size_t __size ));
extern void *(valloc (size_t __size ));
extern int (posix_memalign (void **__memptr , size_t __alignment , size_t __size ));
extern void (abort (void ));
extern int (atexit (void ((*__func )(void ))));
extern int (on_exit (void ((*__func )(int __status , void *__arg )), void *__arg ));
extern void (exit (int __status ));
extern char *(getenv (__const char *__name ));
extern char *(__secure_getenv (__const char *__name ));
extern int (putenv (char *__string ));
extern int (setenv (__const char *__name , __const char *__value , int __replace ));
extern int (unsetenv (__const char *__name ));
extern int (clearenv (void ));
extern char *(mktemp (char *__template ));
extern int (mkstemp (char *__template ));
extern char *(mkdtemp (char *__template ));
extern int (system (__const char *__command ));
extern char *(realpath (__const char *__restrict __name , char *__restrict __resolved ));
typedef int ((*__compar_fn_t )(__const void *, __const void *));
extern void *(bsearch (__const void *__key , __const void *__base , size_t __nmemb , size_t __size , __compar_fn_t __compar ));
extern void (qsort (void *__base , size_t __nmemb , size_t __size , __compar_fn_t __compar ));
extern int (abs (int __x ));
extern long int (labs (long int __x ));
extern div_t (div (int __numer , int __denom ));
extern ldiv_t (ldiv (long int __numer , long int __denom ));
extern char *(ecvt (double __value , int __ndigit , int *__restrict __decpt , int *__restrict __sign ));
extern char *(fcvt (double __value , int __ndigit , int *__restrict __decpt , int *__restrict __sign ));
extern char *(gcvt (double __value , int __ndigit , char *__buf ));
extern char *(qecvt (long double __value , int __ndigit , int *__restrict __decpt , int *__restrict __sign ));
extern char *(qfcvt (long double __value , int __ndigit , int *__restrict __decpt , int *__restrict __sign ));
extern char *(qgcvt (long double __value , int __ndigit , char *__buf ));
extern int (ecvt_r (double __value , int __ndigit , int *__restrict __decpt , int *__restrict __sign , char *__restrict __buf , size_t __len ));
extern int (fcvt_r (double __value , int __ndigit , int *__restrict __decpt , int *__restrict __sign , char *__restrict __buf , size_t __len ));
extern int (qecvt_r (long double __value , int __ndigit , int *__restrict __decpt , int *__restrict __sign , char *__restrict __buf , size_t __len ));
extern int (qfcvt_r (long double __value , int __ndigit , int *__restrict __decpt , int *__restrict __sign , char *__restrict __buf , size_t __len ));
extern int (mblen (__const char *__s , size_t __n ));
extern int (mbtowc (wchar_t *__restrict __pwc , __const char *__restrict __s , size_t __n ));
extern int (wctomb (char *__s , wchar_t __wchar ));
extern size_t (mbstowcs (wchar_t *__restrict __pwcs , __const char *__restrict __s , size_t __n ));
extern size_t (wcstombs (char *__restrict __s , __const wchar_t *__restrict __pwcs , size_t __n ));
extern int (rpmatch (__const char *__response ));
extern int (posix_openpt (int __oflag ));
extern int (getloadavg (double (__loadavg []), int __nelem ));
struct _IO_FILE ;
typedef struct _IO_FILE FILE ;
typedef struct _IO_FILE __FILE ;
typedef struct {
int  __count ;
union {
unsigned  int  __wch ;
char  (__wchb [4]);
}
 __value ;
}
__mbstate_t ;
typedef struct {
__off_t  __pos ;
__mbstate_t  __state ;
}
_G_fpos_t ;
typedef struct {
__off64_t  __pos ;
__mbstate_t  __state ;
}
_G_fpos64_t ;
typedef int _G_int16_t ;
typedef int _G_int32_t ;
typedef unsigned int _G_uint16_t ;
typedef unsigned int _G_uint32_t ;
typedef __builtin_va_list __gnuc_va_list ;
struct _IO_jump_t ;
struct _IO_FILE ;
typedef void _IO_lock_t ;
struct _IO_marker {
struct _IO_marker  *_next ;
struct _IO_FILE  *_sbuf ;
int  _pos ;
}
;
enum __codecvt_result { 
__codecvt_ok , __codecvt_partial , __codecvt_error , __codecvt_noconv 
}
;
struct _IO_FILE {
int  _flags ;
char  *_IO_read_ptr ;
char  *_IO_read_end ;
char  *_IO_read_base ;
char  *_IO_write_base ;
char  *_IO_write_ptr ;
char  *_IO_write_end ;
char  *_IO_buf_base ;
char  *_IO_buf_end ;
char  *_IO_save_base ;
char  *_IO_backup_base ;
char  *_IO_save_end ;
struct _IO_marker  *_markers ;
struct _IO_FILE  *_chain ;
int  _fileno ;
int  _flags2 ;
__off_t  _old_offset ;
unsigned  short  _cur_column ;
signed  char  _vtable_offset ;
char  (_shortbuf [1]);
_IO_lock_t  *_lock ;
__off64_t  _offset ;
void  *__pad1 ;
void  *__pad2 ;
void  *__pad3 ;
void  *__pad4 ;
size_t  __pad5 ;
int  _mode ;
char  (_unused2 [(((15 * sizeof(int )) - (4 * sizeof(void *))) - sizeof(size_t ))]);
}
;
typedef struct _IO_FILE _IO_FILE ;
struct _IO_FILE_plus ;
extern struct _IO_FILE_plus _IO_2_1_stdin_ ;
extern struct _IO_FILE_plus _IO_2_1_stdout_ ;
extern struct _IO_FILE_plus _IO_2_1_stderr_ ;
typedef __ssize_t (__io_read_fn (void *__cookie , char *__buf , size_t __nbytes ));
typedef __ssize_t (__io_write_fn (void *__cookie , __const char *__buf , size_t __n ));
typedef int (__io_seek_fn (void *__cookie , __off64_t *__pos , int __w ));
typedef int (__io_close_fn (void *__cookie ));
extern int (__underflow (_IO_FILE *));
extern int (__uflow (_IO_FILE *));
extern int (__overflow (_IO_FILE *, int ));
extern int (_IO_getc (_IO_FILE *__fp ));
extern int (_IO_putc (int __c , _IO_FILE *__fp ));
extern int (_IO_feof (_IO_FILE *__fp ));
extern int (_IO_ferror (_IO_FILE *__fp ));
extern int (_IO_peekc_locked (_IO_FILE *__fp ));
extern void (_IO_flockfile (_IO_FILE *));
extern void (_IO_funlockfile (_IO_FILE *));
extern int (_IO_ftrylockfile (_IO_FILE *));
extern int (_IO_vfscanf (_IO_FILE *__restrict , const char *__restrict , __gnuc_va_list , int *__restrict ));
extern int (_IO_vfprintf (_IO_FILE *__restrict , const char *__restrict , __gnuc_va_list ));
extern __ssize_t (_IO_padn (_IO_FILE *, int , __ssize_t ));
extern size_t (_IO_sgetn (_IO_FILE *, void *, size_t ));
extern __off64_t (_IO_seekoff (_IO_FILE *, __off64_t , int , int ));
extern __off64_t (_IO_seekpos (_IO_FILE *, __off64_t , int ));
extern void (_IO_free_backup_area (_IO_FILE *));
typedef _G_fpos_t fpos_t ;
extern struct _IO_FILE *stdin ;
extern struct _IO_FILE *stdout ;
extern struct _IO_FILE *stderr ;
extern int (remove (__const char *__filename ));
extern int (rename (__const char *__old , __const char *__new ));
extern FILE *(tmpfile (void ));
extern char *(tmpnam (char *__s ));
extern char *(tmpnam_r (char *__s ));
extern char *(tempnam (__const char *__dir , __const char *__pfx ));
extern int (fclose (FILE *__stream ));
extern int (fflush (FILE *__stream ));
extern int (fflush_unlocked (FILE *__stream ));
extern FILE *(fopen (__const char *__restrict __filename , __const char *__restrict __modes ));
extern FILE *(freopen (__const char *__restrict __filename , __const char *__restrict __modes , FILE *__restrict __stream ));
extern FILE *(fdopen (int __fd , __const char *__modes ));
extern void (setbuf (FILE *__restrict __stream , char *__restrict __buf ));
extern int (setvbuf (FILE *__restrict __stream , char *__restrict __buf , int __modes , size_t __n ));
extern void (setbuffer (FILE *__restrict __stream , char *__restrict __buf , size_t __size ));
extern void (setlinebuf (FILE *__stream ));
extern int (fprintf (FILE *__restrict __stream , __const char *__restrict __format , ... ));
extern int (printf (__const char *__restrict __format , ... ));
extern int (sprintf (char *__restrict __s , __const char *__restrict __format , ... ));
extern int (vfprintf (FILE *__restrict __s , __const char *__restrict __format , __gnuc_va_list __arg ));
extern int (vprintf (__const char *__restrict __format , __gnuc_va_list __arg ));
extern int (vsprintf (char *__restrict __s , __const char *__restrict __format , __gnuc_va_list __arg ));
extern int (snprintf (char *__restrict __s , size_t __maxlen , __const char *__restrict __format , ... ));
extern int (vsnprintf (char *__restrict __s , size_t __maxlen , __const char *__restrict __format , __gnuc_va_list __arg ));
extern int (fscanf (FILE *__restrict __stream , __const char *__restrict __format , ... ));
extern int (scanf (__const char *__restrict __format , ... ));
extern int (sscanf (__const char *__restrict __s , __const char *__restrict __format , ... ));
extern int (fgetc (FILE *__stream ));
extern int (getc (FILE *__stream ));
extern int (getchar (void ));
extern int (getc_unlocked (FILE *__stream ));
extern int (getchar_unlocked (void ));
extern int (fgetc_unlocked (FILE *__stream ));
extern int (fputc (int __c , FILE *__stream ));
extern int (putc (int __c , FILE *__stream ));
extern int (putchar (int __c ));
extern int (fputc_unlocked (int __c , FILE *__stream ));
extern int (putc_unlocked (int __c , FILE *__stream ));
extern int (putchar_unlocked (int __c ));
extern int (getw (FILE *__stream ));
extern int (putw (int __w , FILE *__stream ));
extern char *(fgets (char *__restrict __s , int __n , FILE *__restrict __stream ));
extern char *(gets (char *__s ));
extern int (fputs (__const char *__restrict __s , FILE *__restrict __stream ));
extern int (puts (__const char *__s ));
extern int (ungetc (int __c , FILE *__stream ));
extern size_t (fread (void *__restrict __ptr , size_t __size , size_t __n , FILE *__restrict __stream ));
extern size_t (fwrite (__const void *__restrict __ptr , size_t __size , size_t __n , FILE *__restrict __s ));
extern size_t (fread_unlocked (void *__restrict __ptr , size_t __size , size_t __n , FILE *__restrict __stream ));
extern size_t (fwrite_unlocked (__const void *__restrict __ptr , size_t __size , size_t __n , FILE *__restrict __stream ));
extern int (fseek (FILE *__stream , long int __off , int __whence ));
extern long int (ftell (FILE *__stream ));
extern void (rewind (FILE *__stream ));
extern int (fseeko (FILE *__stream , __off_t __off , int __whence ));
extern __off_t (ftello (FILE *__stream ));
extern int (fgetpos (FILE *__restrict __stream , fpos_t *__restrict __pos ));
extern int (fsetpos (FILE *__stream , __const fpos_t *__pos ));
extern void (clearerr (FILE *__stream ));
extern int (feof (FILE *__stream ));
extern int (ferror (FILE *__stream ));
extern void (clearerr_unlocked (FILE *__stream ));
extern int (feof_unlocked (FILE *__stream ));
extern int (ferror_unlocked (FILE *__stream ));
extern void (perror (__const char *__s ));
extern int sys_nerr ;
extern __const char *__const (sys_errlist []);
extern int (fileno (FILE *__stream ));
extern int (fileno_unlocked (FILE *__stream ));
extern FILE *(popen (__const char *__command , __const char *__modes ));
extern int (pclose (FILE *__stream ));
extern char *(ctermid (char *__s ));
extern void (flockfile (FILE *__stream ));
extern int (ftrylockfile (FILE *__stream ));
extern void (funlockfile (FILE *__stream ));
extern __inline int (vprintf (__const char *__restrict __fmt , __gnuc_va_list __arg ));
extern __inline int vprintf (__const char *__restrict __fmt , __gnuc_va_list __arg ){
return vfprintf(stdout, __fmt, __arg);
}
extern __inline int (getchar (void ));
extern __inline int getchar (void ){
return _IO_getc(stdin);
}
extern __inline int (fgetc_unlocked (FILE *__fp ));
extern __inline int fgetc_unlocked (FILE *__fp ){
return (__builtin_expect(((__fp -> _IO_read_ptr) >= (__fp -> _IO_read_end)), 0) ? __uflow(__fp) : (* ((unsigned char *)(__fp -> _IO_read_ptr++))));
}
extern __inline int (getc_unlocked (FILE *__fp ));
extern __inline int getc_unlocked (FILE *__fp ){
return (__builtin_expect(((__fp -> _IO_read_ptr) >= (__fp -> _IO_read_end)), 0) ? __uflow(__fp) : (* ((unsigned char *)(__fp -> _IO_read_ptr++))));
}
extern __inline int (getchar_unlocked (void ));
extern __inline int getchar_unlocked (void ){
return (__builtin_expect(((stdin -> _IO_read_ptr) >= (stdin -> _IO_read_end)), 0) ? __uflow(stdin) : (* ((unsigned char *)(stdin -> _IO_read_ptr++))));
}
extern __inline int (putchar (int __c ));
extern __inline int putchar (int __c ){
return _IO_putc(__c, stdout);
}
extern __inline int (fputc_unlocked (int __c , FILE *__stream ));
extern __inline int fputc_unlocked (int __c , FILE *__stream ){
return (__builtin_expect(((__stream -> _IO_write_ptr) >= (__stream -> _IO_write_end)), 0) ? __overflow(__stream, ((unsigned char )__c)) : ((unsigned char )((* (__stream -> _IO_write_ptr++)) = __c)));
}
extern __inline int (putc_unlocked (int __c , FILE *__stream ));
extern __inline int putc_unlocked (int __c , FILE *__stream ){
return (__builtin_expect(((__stream -> _IO_write_ptr) >= (__stream -> _IO_write_end)), 0) ? __overflow(__stream, ((unsigned char )__c)) : ((unsigned char )((* (__stream -> _IO_write_ptr++)) = __c)));
}
extern __inline int (putchar_unlocked (int __c ));
extern __inline int putchar_unlocked (int __c ){
return (__builtin_expect(((stdout -> _IO_write_ptr) >= (stdout -> _IO_write_end)), 0) ? __overflow(stdout, ((unsigned char )__c)) : ((unsigned char )((* (stdout -> _IO_write_ptr++)) = __c)));
}
extern __inline int (feof_unlocked (FILE *__stream ));
extern __inline int feof_unlocked (FILE *__stream ){
return (((__stream -> _flags) & 16) != 0);
}
extern __inline int (ferror_unlocked (FILE *__stream ));
extern __inline int ferror_unlocked (FILE *__stream ){
return (((__stream -> _flags) & 32) != 0);
}
extern void *(memcpy (void *__restrict __dest , __const void *__restrict __src , size_t __n ));
extern void *(memmove (void *__dest , __const void *__src , size_t __n ));
extern void *(memccpy (void *__restrict __dest , __const void *__restrict __src , int __c , size_t __n ));
extern void *(memset (void *__s , int __c , size_t __n ));
extern int (memcmp (__const void *__s1 , __const void *__s2 , size_t __n ));
extern void *(memchr (__const void *__s , int __c , size_t __n ));
extern char *(strcpy (char *__restrict __dest , __const char *__restrict __src ));
extern char *(strncpy (char *__restrict __dest , __const char *__restrict __src , size_t __n ));
extern char *(strcat (char *__restrict __dest , __const char *__restrict __src ));
extern char *(strncat (char *__restrict __dest , __const char *__restrict __src , size_t __n ));
extern int (strcmp (__const char *__s1 , __const char *__s2 ));
extern int (strncmp (__const char *__s1 , __const char *__s2 , size_t __n ));
extern int (strcoll (__const char *__s1 , __const char *__s2 ));
extern size_t (strxfrm (char *__restrict __dest , __const char *__restrict __src , size_t __n ));
extern char *(strdup (__const char *__s ));
extern char *(strchr (__const char *__s , int __c ));
extern char *(strrchr (__const char *__s , int __c ));
extern size_t (strcspn (__const char *__s , __const char *__reject ));
extern size_t (strspn (__const char *__s , __const char *__accept ));
extern char *(strpbrk (__const char *__s , __const char *__accept ));
extern char *(strstr (__const char *__haystack , __const char *__needle ));
extern char *(strtok (char *__restrict __s , __const char *__restrict __delim ));
extern char *(__strtok_r (char *__restrict __s , __const char *__restrict __delim , char **__restrict __save_ptr ));
extern char *(strtok_r (char *__restrict __s , __const char *__restrict __delim , char **__restrict __save_ptr ));
extern size_t (strlen (__const char *__s ));
extern char *(strerror (int __errnum ));
extern int (strerror_r (int __errnum , char *__buf , size_t __buflen )) __asm__ ("__xpg_strerror_r");
extern void (__bzero (void *__s , size_t __n ));
extern void (bcopy (__const void *__src , void *__dest , size_t __n ));
extern void (bzero (void *__s , size_t __n ));
extern int (bcmp (__const void *__s1 , __const void *__s2 , size_t __n ));
extern char *(index (__const char *__s , int __c ));
extern char *(rindex (__const char *__s , int __c ));
extern int (ffs (int __i ));
extern int (strcasecmp (__const char *__s1 , __const char *__s2 ));
extern int (strncasecmp (__const char *__s1 , __const char *__s2 , size_t __n ));
extern char *(strsep (char **__restrict __stringp , __const char *__restrict __delim ));
extern void *(__rawmemchr (const void *__s , int __c ));
extern __inline size_t (__strcspn_c1 (__const char *__s , int __reject ));
extern __inline size_t (__strcspn_c1 (__const char *__s , int __reject ));
extern __inline size_t __strcspn_c1 (__const char *__s , int __reject ){
register size_t __result  = 0;
while(((__s[__result]) != '\0') && ((__s[__result]) != __reject))
++ __result;
return __result;
}
extern __inline size_t (__strcspn_c2 (__const char *__s , int __reject1 , int __reject2 ));
extern __inline size_t (__strcspn_c2 (__const char *__s , int __reject1 , int __reject2 ));
extern __inline size_t __strcspn_c2 (__const char *__s , int __reject1 , int __reject2 ){
register size_t __result  = 0;
while((((__s[__result]) != '\0') && ((__s[__result]) != __reject1)) && ((__s[__result]) != __reject2))
++ __result;
return __result;
}
extern __inline size_t (__strcspn_c3 (__const char *__s , int __reject1 , int __reject2 , int __reject3 ));
extern __inline size_t (__strcspn_c3 (__const char *__s , int __reject1 , int __reject2 , int __reject3 ));
extern __inline size_t __strcspn_c3 (__const char *__s , int __reject1 , int __reject2 , int __reject3 ){
register size_t __result  = 0;
while(((((__s[__result]) != '\0') && ((__s[__result]) != __reject1)) && ((__s[__result]) != __reject2)) && ((__s[__result]) != __reject3))
++ __result;
return __result;
}
extern __inline size_t (__strspn_c1 (__const char *__s , int __accept ));
extern __inline size_t (__strspn_c1 (__const char *__s , int __accept ));
extern __inline size_t __strspn_c1 (__const char *__s , int __accept ){
register size_t __result  = 0;
while((__s[__result]) == __accept)
++ __result;
return __result;
}
extern __inline size_t (__strspn_c2 (__const char *__s , int __accept1 , int __accept2 ));
extern __inline size_t (__strspn_c2 (__const char *__s , int __accept1 , int __accept2 ));
extern __inline size_t __strspn_c2 (__const char *__s , int __accept1 , int __accept2 ){
register size_t __result  = 0;
while(((__s[__result]) == __accept1) || ((__s[__result]) == __accept2))
++ __result;
return __result;
}
extern __inline size_t (__strspn_c3 (__const char *__s , int __accept1 , int __accept2 , int __accept3 ));
extern __inline size_t (__strspn_c3 (__const char *__s , int __accept1 , int __accept2 , int __accept3 ));
extern __inline size_t __strspn_c3 (__const char *__s , int __accept1 , int __accept2 , int __accept3 ){
register size_t __result  = 0;
while((((__s[__result]) == __accept1) || ((__s[__result]) == __accept2)) || ((__s[__result]) == __accept3))
++ __result;
return __result;
}
extern __inline char *(__strpbrk_c2 (__const char *__s , int __accept1 , int __accept2 ));
extern __inline char *(__strpbrk_c2 (__const char *__s , int __accept1 , int __accept2 ));
extern __inline char *(__strpbrk_c2 (__const char *__s , int __accept1 , int __accept2 )){
while((((* __s) != '\0') && ((* __s) != __accept1)) && ((* __s) != __accept2))
++ __s;
return (((* __s) == '\0') ? ((void *)0) : ((char *)((size_t )__s)));
}
extern __inline char *(__strpbrk_c3 (__const char *__s , int __accept1 , int __accept2 , int __accept3 ));
extern __inline char *(__strpbrk_c3 (__const char *__s , int __accept1 , int __accept2 , int __accept3 ));
extern __inline char *(__strpbrk_c3 (__const char *__s , int __accept1 , int __accept2 , int __accept3 )){
while(((((* __s) != '\0') && ((* __s) != __accept1)) && ((* __s) != __accept2)) && ((* __s) != __accept3))
++ __s;
return (((* __s) == '\0') ? ((void *)0) : ((char *)((size_t )__s)));
}
extern __inline char *(__strtok_r_1c (char *__s , char __sep , char **__nextp ));
extern __inline char *(__strtok_r_1c (char *__s , char __sep , char **__nextp ));
extern __inline char *(__strtok_r_1c (char *__s , char __sep , char **__nextp )){
char *__result ;
if(__s == ((void *)0))
__s = (* __nextp);
while((* __s) == __sep)
++ __s;
__result = ((void *)0);
if((* __s) != '\0')
{
__result = (__s++);
while((* __s) != '\0')
if((* (__s++)) == __sep)
{
(__s[- 1]) = '\0';
break;
}
(* __nextp) = __s;
}
return __result;
}
extern char *(__strsep_g (char **__stringp , __const char *__delim ));
extern __inline char *(__strsep_1c (char **__s , char __reject ));
extern __inline char *(__strsep_1c (char **__s , char __reject ));
extern __inline char *(__strsep_1c (char **__s , char __reject )){
register char *__retval  = (* __s);
if((__retval != ((void *)0)) && (((* __s) = (((__builtin_constant_p(__reject) && (! __builtin_constant_p(__retval))) && (__reject == '\0')) ? ((char *)__rawmemchr(__retval, __reject)) : __builtin_strchr(__retval, __reject))) != ((void *)0)))
(* (* __s++)) = '\0';
return __retval;
}
extern __inline char *(__strsep_2c (char **__s , char __reject1 , char __reject2 ));
extern __inline char *(__strsep_2c (char **__s , char __reject1 , char __reject2 ));
extern __inline char *(__strsep_2c (char **__s , char __reject1 , char __reject2 )){
register char *__retval  = (* __s);
if(__retval != ((void *)0))
{
register char *__cp  = __retval;
while(1)
{
if((* __cp) == '\0')
{
__cp = ((void *)0);
break;
}
if(((* __cp) == __reject1) || ((* __cp) == __reject2))
{
(* (__cp++)) = '\0';
break;
}
++ __cp;
}
(* __s) = __cp;
}
return __retval;
}
extern __inline char *(__strsep_3c (char **__s , char __reject1 , char __reject2 , char __reject3 ));
extern __inline char *(__strsep_3c (char **__s , char __reject1 , char __reject2 , char __reject3 ));
extern __inline char *(__strsep_3c (char **__s , char __reject1 , char __reject2 , char __reject3 )){
register char *__retval  = (* __s);
if(__retval != ((void *)0))
{
register char *__cp  = __retval;
while(1)
{
if((* __cp) == '\0')
{
__cp = ((void *)0);
break;
}
if((((* __cp) == __reject1) || ((* __cp) == __reject2)) || ((* __cp) == __reject3))
{
(* (__cp++)) = '\0';
break;
}
++ __cp;
}
(* __s) = __cp;
}
return __retval;
}
extern char *(__strdup (__const char *__string ));
extern char *(__strndup (__const char *__string , size_t __n ));
typedef __useconds_t useconds_t ;
typedef __intptr_t intptr_t ;
typedef __socklen_t socklen_t ;
extern int (access (__const char *__name , int __type ));
extern __off_t (lseek (int __fd , __off_t __offset , int __whence ));
extern int (close (int __fd ));
extern ssize_t (read (int __fd , void *__buf , size_t __nbytes ));
extern ssize_t (write (int __fd , __const void *__buf , size_t __n ));
extern int (pipe (int (__pipedes [2])));
extern unsigned int (alarm (unsigned int __seconds ));
extern unsigned int (sleep (unsigned int __seconds ));
extern __useconds_t (ualarm (__useconds_t __value , __useconds_t __interval ));
extern int (usleep (__useconds_t __useconds ));
extern int (pause (void ));
extern int (chown (__const char *__file , __uid_t __owner , __gid_t __group ));
extern int (fchown (int __fd , __uid_t __owner , __gid_t __group ));
extern int (lchown (__const char *__file , __uid_t __owner , __gid_t __group ));
extern int (chdir (__const char *__path ));
extern int (fchdir (int __fd ));
extern char *(getcwd (char *__buf , size_t __size ));
extern char *(getwd (char *__buf ));
extern int (dup (int __fd ));
extern int (dup2 (int __fd , int __fd2 ));
extern char **__environ ;
extern int (execve (__const char *__path , char *__const (__argv []), char *__const (__envp [])));
extern int (execv (__const char *__path , char *__const (__argv [])));
extern int (execle (__const char *__path , __const char *__arg , ... ));
extern int (execl (__const char *__path , __const char *__arg , ... ));
extern int (execvp (__const char *__file , char *__const (__argv [])));
extern int (execlp (__const char *__file , __const char *__arg , ... ));
extern int (nice (int __inc ));
extern void (_exit (int __status ));
enum { 
_PC_LINK_MAX , _PC_MAX_CANON , _PC_MAX_INPUT , _PC_NAME_MAX , _PC_PATH_MAX , _PC_PIPE_BUF , _PC_CHOWN_RESTRICTED , _PC_NO_TRUNC , _PC_VDISABLE , _PC_SYNC_IO , _PC_ASYNC_IO , _PC_PRIO_IO , _PC_SOCK_MAXBUF , _PC_FILESIZEBITS , _PC_REC_INCR_XFER_SIZE , _PC_REC_MAX_XFER_SIZE , _PC_REC_MIN_XFER_SIZE , _PC_REC_XFER_ALIGN , _PC_ALLOC_SIZE_MIN , _PC_SYMLINK_MAX , _PC_2_SYMLINKS 
}
;
enum { 
_SC_ARG_MAX , _SC_CHILD_MAX , _SC_CLK_TCK , _SC_NGROUPS_MAX , _SC_OPEN_MAX , _SC_STREAM_MAX , _SC_TZNAME_MAX , _SC_JOB_CONTROL , _SC_SAVED_IDS , _SC_REALTIME_SIGNALS , _SC_PRIORITY_SCHEDULING , _SC_TIMERS , _SC_ASYNCHRONOUS_IO , _SC_PRIORITIZED_IO , _SC_SYNCHRONIZED_IO , _SC_FSYNC , _SC_MAPPED_FILES , _SC_MEMLOCK , _SC_MEMLOCK_RANGE , _SC_MEMORY_PROTECTION , _SC_MESSAGE_PASSING , _SC_SEMAPHORES , _SC_SHARED_MEMORY_OBJECTS , _SC_AIO_LISTIO_MAX , _SC_AIO_MAX , _SC_AIO_PRIO_DELTA_MAX , _SC_DELAYTIMER_MAX , _SC_MQ_OPEN_MAX , _SC_MQ_PRIO_MAX , _SC_VERSION , _SC_PAGESIZE , _SC_RTSIG_MAX , _SC_SEM_NSEMS_MAX , _SC_SEM_VALUE_MAX , _SC_SIGQUEUE_MAX , _SC_TIMER_MAX , _SC_BC_BASE_MAX , _SC_BC_DIM_MAX , _SC_BC_SCALE_MAX , _SC_BC_STRING_MAX , _SC_COLL_WEIGHTS_MAX , _SC_EQUIV_CLASS_MAX , _SC_EXPR_NEST_MAX , _SC_LINE_MAX , _SC_RE_DUP_MAX , _SC_CHARCLASS_NAME_MAX , _SC_2_VERSION , _SC_2_C_BIND , _SC_2_C_DEV , _SC_2_FORT_DEV , _SC_2_FORT_RUN , _SC_2_SW_DEV , _SC_2_LOCALEDEF , _SC_PII , _SC_PII_XTI , _SC_PII_SOCKET , _SC_PII_INTERNET , _SC_PII_OSI , _SC_POLL , _SC_SELECT , _SC_UIO_MAXIOV , _SC_IOV_MAX = _SC_UIO_MAXIOV, _SC_PII_INTERNET_STREAM , _SC_PII_INTERNET_DGRAM , _SC_PII_OSI_COTS , _SC_PII_OSI_CLTS , _SC_PII_OSI_M , _SC_T_IOV_MAX , _SC_THREADS , _SC_THREAD_SAFE_FUNCTIONS , _SC_GETGR_R_SIZE_MAX , _SC_GETPW_R_SIZE_MAX , _SC_LOGIN_NAME_MAX , _SC_TTY_NAME_MAX , _SC_THREAD_DESTRUCTOR_ITERATIONS , _SC_THREAD_KEYS_MAX , _SC_THREAD_STACK_MIN , _SC_THREAD_THREADS_MAX , _SC_THREAD_ATTR_STACKADDR , _SC_THREAD_ATTR_STACKSIZE , _SC_THREAD_PRIORITY_SCHEDULING , _SC_THREAD_PRIO_INHERIT , _SC_THREAD_PRIO_PROTECT , _SC_THREAD_PROCESS_SHARED , _SC_NPROCESSORS_CONF , _SC_NPROCESSORS_ONLN , _SC_PHYS_PAGES , _SC_AVPHYS_PAGES , _SC_ATEXIT_MAX , _SC_PASS_MAX , _SC_XOPEN_VERSION , _SC_XOPEN_XCU_VERSION , _SC_XOPEN_UNIX , _SC_XOPEN_CRYPT , _SC_XOPEN_ENH_I18N , _SC_XOPEN_SHM , _SC_2_CHAR_TERM , _SC_2_C_VERSION , _SC_2_UPE , _SC_XOPEN_XPG2 , _SC_XOPEN_XPG3 , _SC_XOPEN_XPG4 , _SC_CHAR_BIT , _SC_CHAR_MAX , _SC_CHAR_MIN , _SC_INT_MAX , _SC_INT_MIN , _SC_LONG_BIT , _SC_WORD_BIT , _SC_MB_LEN_MAX , _SC_NZERO , _SC_SSIZE_MAX , _SC_SCHAR_MAX , _SC_SCHAR_MIN , _SC_SHRT_MAX , _SC_SHRT_MIN , _SC_UCHAR_MAX , _SC_UINT_MAX , _SC_ULONG_MAX , _SC_USHRT_MAX , _SC_NL_ARGMAX , _SC_NL_LANGMAX , _SC_NL_MSGMAX , _SC_NL_NMAX , _SC_NL_SETMAX , _SC_NL_TEXTMAX , _SC_XBS5_ILP32_OFF32 , _SC_XBS5_ILP32_OFFBIG , _SC_XBS5_LP64_OFF64 , _SC_XBS5_LPBIG_OFFBIG , _SC_XOPEN_LEGACY , _SC_XOPEN_REALTIME , _SC_XOPEN_REALTIME_THREADS , _SC_ADVISORY_INFO , _SC_BARRIERS , _SC_BASE , _SC_C_LANG_SUPPORT , _SC_C_LANG_SUPPORT_R , _SC_CLOCK_SELECTION , _SC_CPUTIME , _SC_THREAD_CPUTIME , _SC_DEVICE_IO , _SC_DEVICE_SPECIFIC , _SC_DEVICE_SPECIFIC_R , _SC_FD_MGMT , _SC_FIFO , _SC_PIPE , _SC_FILE_ATTRIBUTES , _SC_FILE_LOCKING , _SC_FILE_SYSTEM , _SC_MONOTONIC_CLOCK , _SC_MULTI_PROCESS , _SC_SINGLE_PROCESS , _SC_NETWORKING , _SC_READER_WRITER_LOCKS , _SC_SPIN_LOCKS , _SC_REGEXP , _SC_REGEX_VERSION , _SC_SHELL , _SC_SIGNALS , _SC_SPAWN , _SC_SPORADIC_SERVER , _SC_THREAD_SPORADIC_SERVER , _SC_SYSTEM_DATABASE , _SC_SYSTEM_DATABASE_R , _SC_TIMEOUTS , _SC_TYPED_MEMORY_OBJECTS , _SC_USER_GROUPS , _SC_USER_GROUPS_R , _SC_2_PBS , _SC_2_PBS_ACCOUNTING , _SC_2_PBS_LOCATE , _SC_2_PBS_MESSAGE , _SC_2_PBS_TRACK , _SC_SYMLOOP_MAX , _SC_STREAMS , _SC_2_PBS_CHECKPOINT , _SC_V6_ILP32_OFF32 , _SC_V6_ILP32_OFFBIG , _SC_V6_LP64_OFF64 , _SC_V6_LPBIG_OFFBIG , _SC_HOST_NAME_MAX , _SC_TRACE , _SC_TRACE_EVENT_FILTER , _SC_TRACE_INHERIT , _SC_TRACE_LOG , _SC_LEVEL1_ICACHE_SIZE , _SC_LEVEL1_ICACHE_ASSOC , _SC_LEVEL1_ICACHE_LINESIZE , _SC_LEVEL1_DCACHE_SIZE , _SC_LEVEL1_DCACHE_ASSOC , _SC_LEVEL1_DCACHE_LINESIZE , _SC_LEVEL2_CACHE_SIZE , _SC_LEVEL2_CACHE_ASSOC , _SC_LEVEL2_CACHE_LINESIZE , _SC_LEVEL3_CACHE_SIZE , _SC_LEVEL3_CACHE_ASSOC , _SC_LEVEL3_CACHE_LINESIZE , _SC_LEVEL4_CACHE_SIZE , _SC_LEVEL4_CACHE_ASSOC , _SC_LEVEL4_CACHE_LINESIZE , _SC_IPV6 = (_SC_LEVEL1_ICACHE_SIZE + 50), _SC_RAW_SOCKETS 
}
;
enum { 
_CS_PATH , _CS_V6_WIDTH_RESTRICTED_ENVS , _CS_GNU_LIBC_VERSION , _CS_GNU_LIBPTHREAD_VERSION , _CS_LFS_CFLAGS = 1000, _CS_LFS_LDFLAGS , _CS_LFS_LIBS , _CS_LFS_LINTFLAGS , _CS_LFS64_CFLAGS , _CS_LFS64_LDFLAGS , _CS_LFS64_LIBS , _CS_LFS64_LINTFLAGS , _CS_XBS5_ILP32_OFF32_CFLAGS = 1100, _CS_XBS5_ILP32_OFF32_LDFLAGS , _CS_XBS5_ILP32_OFF32_LIBS , _CS_XBS5_ILP32_OFF32_LINTFLAGS , _CS_XBS5_ILP32_OFFBIG_CFLAGS , _CS_XBS5_ILP32_OFFBIG_LDFLAGS , _CS_XBS5_ILP32_OFFBIG_LIBS , _CS_XBS5_ILP32_OFFBIG_LINTFLAGS , _CS_XBS5_LP64_OFF64_CFLAGS , _CS_XBS5_LP64_OFF64_LDFLAGS , _CS_XBS5_LP64_OFF64_LIBS , _CS_XBS5_LP64_OFF64_LINTFLAGS , _CS_XBS5_LPBIG_OFFBIG_CFLAGS , _CS_XBS5_LPBIG_OFFBIG_LDFLAGS , _CS_XBS5_LPBIG_OFFBIG_LIBS , _CS_XBS5_LPBIG_OFFBIG_LINTFLAGS , _CS_POSIX_V6_ILP32_OFF32_CFLAGS , _CS_POSIX_V6_ILP32_OFF32_LDFLAGS , _CS_POSIX_V6_ILP32_OFF32_LIBS , _CS_POSIX_V6_ILP32_OFF32_LINTFLAGS , _CS_POSIX_V6_ILP32_OFFBIG_CFLAGS , _CS_POSIX_V6_ILP32_OFFBIG_LDFLAGS , _CS_POSIX_V6_ILP32_OFFBIG_LIBS , _CS_POSIX_V6_ILP32_OFFBIG_LINTFLAGS , _CS_POSIX_V6_LP64_OFF64_CFLAGS , _CS_POSIX_V6_LP64_OFF64_LDFLAGS , _CS_POSIX_V6_LP64_OFF64_LIBS , _CS_POSIX_V6_LP64_OFF64_LINTFLAGS , _CS_POSIX_V6_LPBIG_OFFBIG_CFLAGS , _CS_POSIX_V6_LPBIG_OFFBIG_LDFLAGS , _CS_POSIX_V6_LPBIG_OFFBIG_LIBS , _CS_POSIX_V6_LPBIG_OFFBIG_LINTFLAGS 
}
;
extern long int (pathconf (__const char *__path , int __name ));
extern long int (fpathconf (int __fd , int __name ));
extern long int (sysconf (int __name ));
extern size_t (confstr (int __name , char *__buf , size_t __len ));
extern __pid_t (getpid (void ));
extern __pid_t (getppid (void ));
extern __pid_t (getpgrp (void ));
extern __pid_t (__getpgid (__pid_t __pid ));
extern int (setpgid (__pid_t __pid , __pid_t __pgid ));
extern int (setpgrp (void ));
extern __pid_t (setsid (void ));
extern __uid_t (getuid (void ));
extern __uid_t (geteuid (void ));
extern __gid_t (getgid (void ));
extern __gid_t (getegid (void ));
extern int (getgroups (int __size , __gid_t (__list [])));
extern int (setuid (__uid_t __uid ));
extern int (setreuid (__uid_t __ruid , __uid_t __euid ));
extern int (seteuid (__uid_t __uid ));
extern int (setgid (__gid_t __gid ));
extern int (setregid (__gid_t __rgid , __gid_t __egid ));
extern int (setegid (__gid_t __gid ));
extern __pid_t (fork (void ));
extern __pid_t (vfork (void ));
extern char *(ttyname (int __fd ));
extern int (ttyname_r (int __fd , char *__buf , size_t __buflen ));
extern int (isatty (int __fd ));
extern int (ttyslot (void ));
extern int (link (__const char *__from , __const char *__to ));
extern int (symlink (__const char *__from , __const char *__to ));
extern ssize_t (readlink (__const char *__restrict __path , char *__restrict __buf , size_t __len ));
extern int (unlink (__const char *__name ));
extern int (rmdir (__const char *__path ));
extern __pid_t (tcgetpgrp (int __fd ));
extern int (tcsetpgrp (int __fd , __pid_t __pgrp_id ));
extern char *(getlogin (void ));
extern int (getlogin_r (char *__name , size_t __name_len ));
extern int (setlogin (__const char *__name ));
extern char *optarg ;
extern int optind ;
extern int opterr ;
extern int optopt ;
extern int (getopt (int ___argc , char *const *___argv , const char *__shortopts ));
extern int (gethostname (char *__name , size_t __len ));
extern int (sethostname (__const char *__name , size_t __len ));
extern int (sethostid (long int __id ));
extern int (getdomainname (char *__name , size_t __len ));
extern int (setdomainname (__const char *__name , size_t __len ));
extern int (vhangup (void ));
extern int (revoke (__const char *__file ));
extern int (profil (unsigned short int *__sample_buffer , size_t __size , size_t __offset , unsigned int __scale ));
extern int (acct (__const char *__name ));
extern char *(getusershell (void ));
extern void (endusershell (void ));
extern void (setusershell (void ));
extern int (daemon (int __nochdir , int __noclose ));
extern int (chroot (__const char *__path ));
extern char *(getpass (__const char *__prompt ));
extern int (fsync (int __fd ));
extern long int (gethostid (void ));
extern void (sync (void ));
extern int (getpagesize (void ));
extern int (getdtablesize (void ));
extern int (truncate (__const char *__file , __off_t __length ));
extern int (ftruncate (int __fd , __off_t __length ));
extern int (brk (void *__addr ));
extern void *(sbrk (intptr_t __delta ));
extern long int (syscall (long int __sysno , ... ));
extern int (lockf (int __fd , int __cmd , __off_t __len ));
extern int (fdatasync (int __fildes ));
extern int *(__errno_location (void ));
struct flock {
short  int  l_type ;
short  int  l_whence ;
__off_t  l_start ;
__off_t  l_len ;
__pid_t  l_pid ;
}
;
extern int (fcntl (int __fd , int __cmd , ... ));
extern int (open (__const char *__file , int __oflag , ... ));
extern int (creat (__const char *__file , __mode_t __mode ));
extern int (posix_fadvise (int __fd , __off_t __offset , __off_t __len , int __advise ));
extern int (posix_fallocate (int __fd , __off_t __offset , __off_t __len ));
struct iovec {
void  *iov_base ;
size_t  iov_len ;
}
;
extern ssize_t (readv (int __fd , __const struct iovec *__iovec , int __count ));
extern ssize_t (writev (int __fd , __const struct iovec *__iovec , int __count ));
enum __socket_type { 
SOCK_STREAM = 1, SOCK_DGRAM = 2, SOCK_RAW = 3, SOCK_RDM = 4, SOCK_SEQPACKET = 5, SOCK_PACKET = 10
}
;
typedef unsigned short int sa_family_t ;
struct sockaddr {
sa_family_t  sa_family ;
char  (sa_data [14]);
}
;
struct sockaddr_storage {
sa_family_t  ss_family ;
__uint32_t  __ss_align ;
char  (__ss_padding [(128 - (2 * sizeof(__uint32_t )))]);
}
;
enum { 
MSG_OOB = 1, MSG_PEEK = 2, MSG_DONTROUTE = 4, MSG_CTRUNC = 8, MSG_PROXY = 16, MSG_TRUNC = 32, MSG_DONTWAIT = 64, MSG_EOR = 128, MSG_WAITALL = 256, MSG_FIN = 512, MSG_SYN = 1024, MSG_CONFIRM = 2048, MSG_RST = 4096, MSG_ERRQUEUE = 8192, MSG_NOSIGNAL = 16384, MSG_MORE = 32768, MSG_CMSG_CLOEXEC = 1073741824
}
;
struct msghdr {
void  *msg_name ;
socklen_t  msg_namelen ;
struct iovec  *msg_iov ;
size_t  msg_iovlen ;
void  *msg_control ;
size_t  msg_controllen ;
int  msg_flags ;
}
;
struct cmsghdr {
size_t  cmsg_len ;
int  cmsg_level ;
int  cmsg_type ;
unsigned  char  (__cmsg_data []);
}
;
extern struct cmsghdr *(__cmsg_nxthdr (struct msghdr *__mhdr , struct cmsghdr *__cmsg ));
extern __inline struct cmsghdr *(__cmsg_nxthdr (struct msghdr *__mhdr , struct cmsghdr *__cmsg ));
extern __inline struct cmsghdr *(__cmsg_nxthdr (struct msghdr *__mhdr , struct cmsghdr *__cmsg )){
if(((size_t )(__cmsg -> cmsg_len)) < sizeof(struct cmsghdr ))
return 0;
__cmsg = ((struct cmsghdr *)(((unsigned char *)__cmsg) + ((((__cmsg -> cmsg_len) + sizeof(size_t )) - 1) & ((size_t )(~ (sizeof(size_t ) - 1))))));
if((((unsigned char *)(__cmsg + 1)) > (((unsigned char *)(__mhdr -> msg_control)) + (__mhdr -> msg_controllen))) || ((((unsigned char *)__cmsg) + ((((__cmsg -> cmsg_len) + sizeof(size_t )) - 1) & ((size_t )(~ (sizeof(size_t ) - 1))))) > (((unsigned char *)(__mhdr -> msg_control)) + (__mhdr -> msg_controllen))))
return 0;
return __cmsg;
}
enum { 
SCM_RIGHTS = 1, SCM_CREDENTIALS = 2
}
;
struct ucred {
pid_t  pid ;
uid_t  uid ;
gid_t  gid ;
}
;
struct linger {
int  l_onoff ;
int  l_linger ;
}
;
struct osockaddr {
unsigned  short  int  sa_family ;
unsigned  char  (sa_data [14]);
}
;
enum { 
SHUT_RD = 0, SHUT_WR , SHUT_RDWR 
}
;
extern int (socket (int __domain , int __type , int __protocol ));
extern int (socketpair (int __domain , int __type , int __protocol , int (__fds [2])));
extern int (bind (int __fd , __const struct sockaddr *__addr , socklen_t __len ));
extern int (getsockname (int __fd , struct sockaddr *__restrict __addr , socklen_t *__restrict __len ));
extern int (connect (int __fd , __const struct sockaddr *__addr , socklen_t __len ));
extern int (getpeername (int __fd , struct sockaddr *__restrict __addr , socklen_t *__restrict __len ));
extern ssize_t (send (int __fd , __const void *__buf , size_t __n , int __flags ));
extern ssize_t (recv (int __fd , void *__buf , size_t __n , int __flags ));
extern ssize_t (sendto (int __fd , __const void *__buf , size_t __n , int __flags , __const struct sockaddr *__addr , socklen_t __addr_len ));
extern ssize_t (recvfrom (int __fd , void *__restrict __buf , size_t __n , int __flags , struct sockaddr *__restrict __addr , socklen_t *__restrict __addr_len ));
extern ssize_t (sendmsg (int __fd , __const struct msghdr *__message , int __flags ));
extern ssize_t (recvmsg (int __fd , struct msghdr *__message , int __flags ));
extern int (getsockopt (int __fd , int __level , int __optname , void *__restrict __optval , socklen_t *__restrict __optlen ));
extern int (setsockopt (int __fd , int __level , int __optname , __const void *__optval , socklen_t __optlen ));
extern int (listen (int __fd , int __n ));
extern int (accept (int __fd , struct sockaddr *__restrict __addr , socklen_t *__restrict __addr_len ));
extern int (shutdown (int __fd , int __how ));
extern int (sockatmark (int __fd ));
extern int (isfdtype (int __fd , int __fdtype ));
typedef unsigned char uint8_t ;
typedef unsigned short int uint16_t ;
typedef unsigned int uint32_t ;
typedef unsigned long long int uint64_t ;
typedef signed char int_least8_t ;
typedef short int int_least16_t ;
typedef int int_least32_t ;
typedef long long int int_least64_t ;
typedef unsigned char uint_least8_t ;
typedef unsigned short int uint_least16_t ;
typedef unsigned int uint_least32_t ;
typedef unsigned long long int uint_least64_t ;
typedef signed char int_fast8_t ;
typedef int int_fast16_t ;
typedef int int_fast32_t ;
typedef long long int int_fast64_t ;
typedef unsigned char uint_fast8_t ;
typedef unsigned int uint_fast16_t ;
typedef unsigned int uint_fast32_t ;
typedef unsigned long long int uint_fast64_t ;
typedef unsigned int uintptr_t ;
typedef long long int intmax_t ;
typedef unsigned long long int uintmax_t ;
enum { 
IPPROTO_IP = 0, IPPROTO_HOPOPTS = 0, IPPROTO_ICMP = 1, IPPROTO_IGMP = 2, IPPROTO_IPIP = 4, IPPROTO_TCP = 6, IPPROTO_EGP = 8, IPPROTO_PUP = 12, IPPROTO_UDP = 17, IPPROTO_IDP = 22, IPPROTO_TP = 29, IPPROTO_IPV6 = 41, IPPROTO_ROUTING = 43, IPPROTO_FRAGMENT = 44, IPPROTO_RSVP = 46, IPPROTO_GRE = 47, IPPROTO_ESP = 50, IPPROTO_AH = 51, IPPROTO_ICMPV6 = 58, IPPROTO_NONE = 59, IPPROTO_DSTOPTS = 60, IPPROTO_MTP = 92, IPPROTO_ENCAP = 98, IPPROTO_PIM = 103, IPPROTO_COMP = 108, IPPROTO_SCTP = 132, IPPROTO_RAW = 255, IPPROTO_MAX 
}
;
typedef uint16_t in_port_t ;
enum { 
IPPORT_ECHO = 7, IPPORT_DISCARD = 9, IPPORT_SYSTAT = 11, IPPORT_DAYTIME = 13, IPPORT_NETSTAT = 15, IPPORT_FTP = 21, IPPORT_TELNET = 23, IPPORT_SMTP = 25, IPPORT_TIMESERVER = 37, IPPORT_NAMESERVER = 42, IPPORT_WHOIS = 43, IPPORT_MTP = 57, IPPORT_TFTP = 69, IPPORT_RJE = 77, IPPORT_FINGER = 79, IPPORT_TTYLINK = 87, IPPORT_SUPDUP = 95, IPPORT_EXECSERVER = 512, IPPORT_LOGINSERVER = 513, IPPORT_CMDSERVER = 514, IPPORT_EFSSERVER = 520, IPPORT_BIFFUDP = 512, IPPORT_WHOSERVER = 513, IPPORT_ROUTESERVER = 520, IPPORT_RESERVED = 1024, IPPORT_USERRESERVED = 5000
}
;
typedef uint32_t in_addr_t ;
struct in_addr {
in_addr_t  s_addr ;
}
;
struct in6_addr {
union {
uint8_t  (u6_addr8 [16]);
uint16_t  (u6_addr16 [8]);
uint32_t  (u6_addr32 [4]);
}
 in6_u ;
}
;
extern const struct in6_addr in6addr_any ;
extern const struct in6_addr in6addr_loopback ;
struct sockaddr_in {
sa_family_t  sin_family ;
in_port_t  sin_port ;
struct in_addr  sin_addr ;
unsigned  char  (sin_zero [(((sizeof(struct sockaddr ) - sizeof(unsigned short int )) - sizeof(in_port_t )) - sizeof(struct in_addr ))]);
}
;
struct sockaddr_in6 {
sa_family_t  sin6_family ;
in_port_t  sin6_port ;
uint32_t  sin6_flowinfo ;
struct in6_addr  sin6_addr ;
uint32_t  sin6_scope_id ;
}
;
struct ip_mreq {
struct in_addr  imr_multiaddr ;
struct in_addr  imr_interface ;
}
;
struct ip_mreq_source {
struct in_addr  imr_multiaddr ;
struct in_addr  imr_interface ;
struct in_addr  imr_sourceaddr ;
}
;
struct ipv6_mreq {
struct in6_addr  ipv6mr_multiaddr ;
unsigned  int  ipv6mr_interface ;
}
;
struct group_req {
uint32_t  gr_interface ;
struct sockaddr_storage  gr_group ;
}
;
struct group_source_req {
uint32_t  gsr_interface ;
struct sockaddr_storage  gsr_group ;
struct sockaddr_storage  gsr_source ;
}
;
struct ip_msfilter {
struct in_addr  imsf_multiaddr ;
struct in_addr  imsf_interface ;
uint32_t  imsf_fmode ;
uint32_t  imsf_numsrc ;
struct in_addr  (imsf_slist [1]);
}
;
struct group_filter {
uint32_t  gf_interface ;
struct sockaddr_storage  gf_group ;
uint32_t  gf_fmode ;
uint32_t  gf_numsrc ;
struct sockaddr_storage  (gf_slist [1]);
}
;
struct ip_opts {
struct in_addr  ip_dst ;
char  (ip_opts [40]);
}
;
struct ip_mreqn {
struct in_addr  imr_multiaddr ;
struct in_addr  imr_address ;
int  imr_ifindex ;
}
;
struct in_pktinfo {
int  ipi_ifindex ;
struct in_addr  ipi_spec_dst ;
struct in_addr  ipi_addr ;
}
;
extern uint32_t (ntohl (uint32_t __netlong ));
extern uint16_t (ntohs (uint16_t __netshort ));
extern uint32_t (htonl (uint32_t __hostlong ));
extern uint16_t (htons (uint16_t __hostshort ));
extern int (bindresvport (int __sockfd , struct sockaddr_in *__sock_in ));
extern int (bindresvport6 (int __sockfd , struct sockaddr_in6 *__sock_in ));
struct in6_pktinfo {
struct in6_addr  ipi6_addr ;
unsigned  int  ipi6_ifindex ;
}
;
struct ip6_mtuinfo {
struct sockaddr_in6  ip6m_addr ;
uint32_t  ip6m_mtu ;
}
;
struct tcphdr {
u_int16_t  source ;
u_int16_t  dest ;
u_int32_t  seq ;
u_int32_t  ack_seq ;
u_int16_t  res1 :4;
u_int16_t  doff :4;
u_int16_t  fin :1;
u_int16_t  syn :1;
u_int16_t  rst :1;
u_int16_t  psh :1;
u_int16_t  ack :1;
u_int16_t  urg :1;
u_int16_t  res2 :2;
u_int16_t  window ;
u_int16_t  check ;
u_int16_t  urg_ptr ;
}
;
enum { 
TCP_ESTABLISHED = 1, TCP_SYN_SENT , TCP_SYN_RECV , TCP_FIN_WAIT1 , TCP_FIN_WAIT2 , TCP_TIME_WAIT , TCP_CLOSE , TCP_CLOSE_WAIT , TCP_LAST_ACK , TCP_LISTEN , TCP_CLOSING 
}
;
enum tcp_ca_state { 
TCP_CA_Open = 0, TCP_CA_Disorder = 1, TCP_CA_CWR = 2, TCP_CA_Recovery = 3, TCP_CA_Loss = 4
}
;
struct tcp_info {
u_int8_t  tcpi_state ;
u_int8_t  tcpi_ca_state ;
u_int8_t  tcpi_retransmits ;
u_int8_t  tcpi_probes ;
u_int8_t  tcpi_backoff ;
u_int8_t  tcpi_options ;
u_int8_t  tcpi_snd_wscale :4, tcpi_rcv_wscale :4;
u_int32_t  tcpi_rto ;
u_int32_t  tcpi_ato ;
u_int32_t  tcpi_snd_mss ;
u_int32_t  tcpi_rcv_mss ;
u_int32_t  tcpi_unacked ;
u_int32_t  tcpi_sacked ;
u_int32_t  tcpi_lost ;
u_int32_t  tcpi_retrans ;
u_int32_t  tcpi_fackets ;
u_int32_t  tcpi_last_data_sent ;
u_int32_t  tcpi_last_ack_sent ;
u_int32_t  tcpi_last_data_recv ;
u_int32_t  tcpi_last_ack_recv ;
u_int32_t  tcpi_pmtu ;
u_int32_t  tcpi_rcv_ssthresh ;
u_int32_t  tcpi_rtt ;
u_int32_t  tcpi_rttvar ;
u_int32_t  tcpi_snd_ssthresh ;
u_int32_t  tcpi_snd_cwnd ;
u_int32_t  tcpi_advmss ;
u_int32_t  tcpi_reordering ;
u_int32_t  tcpi_rcv_rtt ;
u_int32_t  tcpi_rcv_space ;
u_int32_t  tcpi_total_retrans ;
}
;
struct tcp_md5sig {
struct sockaddr_storage  tcpm_addr ;
u_int16_t  __tcpm_pad1 ;
u_int16_t  tcpm_keylen ;
u_int32_t  __tcpm_pad2 ;
u_int8_t  (tcpm_key [80]);
}
;
extern int (__sigismember (__const __sigset_t *, int ));
extern int (__sigaddset (__sigset_t *, int ));
extern int (__sigdelset (__sigset_t *, int ));
extern __inline int (__sigismember (__const __sigset_t *__set , int __sig ));
extern __inline int __sigismember (__const __sigset_t *__set , int __sig ){
unsigned long int __mask  = (((unsigned long int )1) << ((__sig - 1) % (8 * sizeof(unsigned long int ))));
unsigned long int __word  = ((__sig - 1) / (8 * sizeof(unsigned long int )));
return (((__set -> __val[__word]) & __mask) ? 1 : 0);
}
extern __inline int (__sigaddset (__sigset_t *__set , int __sig ));
extern __inline int __sigaddset (__sigset_t *__set , int __sig ){
unsigned long int __mask  = (((unsigned long int )1) << ((__sig - 1) % (8 * sizeof(unsigned long int ))));
unsigned long int __word  = ((__sig - 1) / (8 * sizeof(unsigned long int )));
return (((__set -> __val[__word]) |= __mask) , 0);
}
extern __inline int (__sigdelset (__sigset_t *__set , int __sig ));
extern __inline int __sigdelset (__sigset_t *__set , int __sig ){
unsigned long int __mask  = (((unsigned long int )1) << ((__sig - 1) % (8 * sizeof(unsigned long int ))));
unsigned long int __word  = ((__sig - 1) / (8 * sizeof(unsigned long int )));
return (((__set -> __val[__word]) &= (~ __mask)) , 0);
}
typedef __sig_atomic_t sig_atomic_t ;
typedef void ((*__sighandler_t )(int ));
extern __sighandler_t (__sysv_signal (int __sig , __sighandler_t __handler ));
extern __sighandler_t (signal (int __sig , __sighandler_t __handler ));
extern int (kill (__pid_t __pid , int __sig ));
extern int (killpg (__pid_t __pgrp , int __sig ));
extern int (raise (int __sig ));
extern __sighandler_t (ssignal (int __sig , __sighandler_t __handler ));
extern int (gsignal (int __sig ));
extern void (psignal (int __sig , __const char *__s ));
extern int (__sigpause (int __sig_or_mask , int __is_sig ));
extern int (sigblock (int __mask ));
extern int (sigsetmask (int __mask ));
extern int (siggetmask (void ));
typedef __sighandler_t sig_t ;
typedef union sigval {
int  sival_int ;
void  *sival_ptr ;
}
sigval_t ;
typedef struct siginfo {
int  si_signo ;
int  si_errno ;
int  si_code ;
union {
int  (_pad [((128 / sizeof(int )) - 3)]);
struct {
__pid_t  si_pid ;
__uid_t  si_uid ;
}
 _kill ;
struct {
int  si_tid ;
int  si_overrun ;
sigval_t  si_sigval ;
}
 _timer ;
struct {
__pid_t  si_pid ;
__uid_t  si_uid ;
sigval_t  si_sigval ;
}
 _rt ;
struct {
__pid_t  si_pid ;
__uid_t  si_uid ;
int  si_status ;
__clock_t  si_utime ;
__clock_t  si_stime ;
}
 _sigchld ;
struct {
void  *si_addr ;
}
 _sigfault ;
struct {
long  int  si_band ;
int  si_fd ;
}
 _sigpoll ;
}
 _sifields ;
}
siginfo_t ;
enum { 
SI_ASYNCNL = (- 60), SI_TKILL = (- 6), SI_SIGIO , SI_ASYNCIO , SI_MESGQ , SI_TIMER , SI_QUEUE , SI_USER , SI_KERNEL = 128
}
;
enum { 
ILL_ILLOPC = 1, ILL_ILLOPN , ILL_ILLADR , ILL_ILLTRP , ILL_PRVOPC , ILL_PRVREG , ILL_COPROC , ILL_BADSTK 
}
;
enum { 
FPE_INTDIV = 1, FPE_INTOVF , FPE_FLTDIV , FPE_FLTOVF , FPE_FLTUND , FPE_FLTRES , FPE_FLTINV , FPE_FLTSUB 
}
;
enum { 
SEGV_MAPERR = 1, SEGV_ACCERR 
}
;
enum { 
BUS_ADRALN = 1, BUS_ADRERR , BUS_OBJERR 
}
;
enum { 
TRAP_BRKPT = 1, TRAP_TRACE 
}
;
enum { 
CLD_EXITED = 1, CLD_KILLED , CLD_DUMPED , CLD_TRAPPED , CLD_STOPPED , CLD_CONTINUED 
}
;
enum { 
POLL_IN = 1, POLL_OUT , POLL_MSG , POLL_ERR , POLL_PRI , POLL_HUP 
}
;
typedef struct sigevent {
sigval_t  sigev_value ;
int  sigev_signo ;
int  sigev_notify ;
union {
int  (_pad [((64 / sizeof(int )) - 3)]);
__pid_t  _tid ;
struct {
void  ((*_function )(sigval_t ));
void  *_attribute ;
}
 _sigev_thread ;
}
 _sigev_un ;
}
sigevent_t ;
enum { 
SIGEV_SIGNAL = 0, SIGEV_NONE , SIGEV_THREAD , SIGEV_THREAD_ID = 4
}
;
extern int (sigemptyset (sigset_t *__set ));
extern int (sigfillset (sigset_t *__set ));
extern int (sigaddset (sigset_t *__set , int __signo ));
extern int (sigdelset (sigset_t *__set , int __signo ));
extern int (sigismember (__const sigset_t *__set , int __signo ));
struct sigaction {
union {
__sighandler_t  sa_handler ;
void  ((*sa_sigaction )(int , siginfo_t *, void *));
}
 __sigaction_handler ;
__sigset_t  sa_mask ;
int  sa_flags ;
void  ((*sa_restorer )(void ));
}
;
extern int (sigprocmask (int __how , __const sigset_t *__restrict __set , sigset_t *__restrict __oset ));
extern int (sigsuspend (__const sigset_t *__set ));
extern int (sigaction (int __sig , __const struct sigaction *__restrict __act , struct sigaction *__restrict __oact ));
extern int (sigpending (sigset_t *__set ));
extern int (sigwait (__const sigset_t *__restrict __set , int *__restrict __sig ));
extern int (sigwaitinfo (__const sigset_t *__restrict __set , siginfo_t *__restrict __info ));
extern int (sigtimedwait (__const sigset_t *__restrict __set , siginfo_t *__restrict __info , __const struct timespec *__restrict __timeout ));
extern int (sigqueue (__pid_t __pid , int __sig , __const union sigval __val ));
extern __const char *__const (_sys_siglist [65]);
extern __const char *__const (sys_siglist [65]);
struct sigvec {
__sighandler_t  sv_handler ;
int  sv_mask ;
int  sv_flags ;
}
;
extern int (sigvec (int __sig , __const struct sigvec *__vec , struct sigvec *__ovec ));
struct _fpreg {
unsigned  short  (significand [4]);
unsigned  short  exponent ;
}
;
struct _fpxreg {
unsigned  short  (significand [4]);
unsigned  short  exponent ;
unsigned  short  (padding [3]);
}
;
struct _xmmreg {
__uint32_t  (element [4]);
}
;
struct _fpstate {
__uint32_t  cw ;
__uint32_t  sw ;
__uint32_t  tag ;
__uint32_t  ipoff ;
__uint32_t  cssel ;
__uint32_t  dataoff ;
__uint32_t  datasel ;
struct _fpreg  (_st [8]);
unsigned  short  status ;
unsigned  short  magic ;
__uint32_t  (_fxsr_env [6]);
__uint32_t  mxcsr ;
__uint32_t  reserved ;
struct _fpxreg  (_fxsr_st [8]);
struct _xmmreg  (_xmm [8]);
__uint32_t  (padding [56]);
}
;
struct sigcontext {
unsigned  short  gs , __gsh ;
unsigned  short  fs , __fsh ;
unsigned  short  es , __esh ;
unsigned  short  ds , __dsh ;
unsigned  long  edi ;
unsigned  long  esi ;
unsigned  long  ebp ;
unsigned  long  esp ;
unsigned  long  ebx ;
unsigned  long  edx ;
unsigned  long  ecx ;
unsigned  long  eax ;
unsigned  long  trapno ;
unsigned  long  err ;
unsigned  long  eip ;
unsigned  short  cs , __csh ;
unsigned  long  eflags ;
unsigned  long  esp_at_signal ;
unsigned  short  ss , __ssh ;
struct _fpstate  *fpstate ;
unsigned  long  oldmask ;
unsigned  long  cr2 ;
}
;
extern int (sigreturn (struct sigcontext *__scp ));
extern int (siginterrupt (int __sig , int __interrupt ));
struct sigstack {
void  *ss_sp ;
int  ss_onstack ;
}
;
enum { 
SS_ONSTACK = 1, SS_DISABLE 
}
;
typedef struct sigaltstack {
void  *ss_sp ;
int  ss_flags ;
size_t  ss_size ;
}
stack_t ;
extern int (sigstack (struct sigstack *__ss , struct sigstack *__oss ));
extern int (sigaltstack (__const struct sigaltstack *__restrict __ss , struct sigaltstack *__restrict __oss ));
extern int (pthread_sigmask (int __how , __const __sigset_t *__restrict __newmask , __sigset_t *__restrict __oldmask ));
extern int (pthread_kill (pthread_t __threadid , int __signo ));
extern int (__libc_current_sigrtmin (void ));
extern int (__libc_current_sigrtmax (void ));
typedef struct cpc_barrier cpc_barrier ;
cpc_barrier *(cpc_barrier_get (int count ));
struct my_cpc__cpc_barrier_await_arglist {
cpc_barrier  *barrier ;
}
;
static inline struct cpc_continuation *(my_cpc__cpc_barrier_await_new_arglist (cpc_barrier *barrier , struct cpc_continuation *my_cpc__continuation_86 )){
struct my_cpc__cpc_barrier_await_arglist *my_cpc__arglist_87 ;
my_cpc__arglist_87 = cpc_alloc((& my_cpc__continuation_86), sizeof(struct my_cpc__cpc_barrier_await_arglist ));
(my_cpc__arglist_87 -> barrier) = barrier;
return my_cpc__continuation_86;
}
void (cpc_barrier_await (struct cpc_continuation *cpc_current_continuation ));
int (cpc_setup_descriptor (int fd , int nonagle ));
struct my_cpc__cpc_write_arglist {
int  fd ;
void  *buf ;
size_t  count ;
}
;
static inline struct cpc_continuation *(my_cpc__cpc_write_new_arglist (int fd , void *buf , size_t count , struct cpc_continuation *my_cpc__continuation_88 )){
struct my_cpc__cpc_write_arglist *my_cpc__arglist_89 ;
my_cpc__arglist_89 = cpc_alloc((& my_cpc__continuation_88), sizeof(struct my_cpc__cpc_write_arglist ));
(my_cpc__arglist_89 -> fd) = fd;
(my_cpc__arglist_89 -> buf) = buf;
(my_cpc__arglist_89 -> count) = count;
return my_cpc__continuation_88;
}
void (cpc_write (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__cpc_write_timeout_arglist {
int  fd ;
void  *buf ;
size_t  count ;
int  secs ;
int  micros ;
}
;
static inline struct cpc_continuation *(my_cpc__cpc_write_timeout_new_arglist (int fd , void *buf , size_t count , int secs , int micros , struct cpc_continuation *my_cpc__continuation_90 )){
struct my_cpc__cpc_write_timeout_arglist *my_cpc__arglist_91 ;
my_cpc__arglist_91 = cpc_alloc((& my_cpc__continuation_90), sizeof(struct my_cpc__cpc_write_timeout_arglist ));
(my_cpc__arglist_91 -> fd) = fd;
(my_cpc__arglist_91 -> buf) = buf;
(my_cpc__arglist_91 -> count) = count;
(my_cpc__arglist_91 -> secs) = secs;
(my_cpc__arglist_91 -> micros) = micros;
return my_cpc__continuation_90;
}
void (cpc_write_timeout (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__cpc_read_arglist {
int  fd ;
void  *buf ;
size_t  count ;
}
;
static inline struct cpc_continuation *(my_cpc__cpc_read_new_arglist (int fd , void *buf , size_t count , struct cpc_continuation *my_cpc__continuation_92 )){
struct my_cpc__cpc_read_arglist *my_cpc__arglist_93 ;
my_cpc__arglist_93 = cpc_alloc((& my_cpc__continuation_92), sizeof(struct my_cpc__cpc_read_arglist ));
(my_cpc__arglist_93 -> fd) = fd;
(my_cpc__arglist_93 -> buf) = buf;
(my_cpc__arglist_93 -> count) = count;
return my_cpc__continuation_92;
}
void (cpc_read (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__cpc_read_timeout_arglist {
int  fd ;
void  *buf ;
size_t  count ;
int  secs ;
int  micros ;
}
;
static inline struct cpc_continuation *(my_cpc__cpc_read_timeout_new_arglist (int fd , void *buf , size_t count , int secs , int micros , struct cpc_continuation *my_cpc__continuation_94 )){
struct my_cpc__cpc_read_timeout_arglist *my_cpc__arglist_95 ;
my_cpc__arglist_95 = cpc_alloc((& my_cpc__continuation_94), sizeof(struct my_cpc__cpc_read_timeout_arglist ));
(my_cpc__arglist_95 -> fd) = fd;
(my_cpc__arglist_95 -> buf) = buf;
(my_cpc__arglist_95 -> count) = count;
(my_cpc__arglist_95 -> secs) = secs;
(my_cpc__arglist_95 -> micros) = micros;
return my_cpc__continuation_94;
}
void (cpc_read_timeout (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__accept_connection_arglist {
int  fd ;
}
;
static inline struct cpc_continuation *(my_cpc__accept_connection_new_arglist (int fd , struct cpc_continuation *my_cpc__continuation_96 )){
struct my_cpc__accept_connection_arglist *my_cpc__arglist_97 ;
my_cpc__arglist_97 = cpc_alloc((& my_cpc__continuation_96), sizeof(struct my_cpc__accept_connection_arglist ));
(my_cpc__arglist_97 -> fd) = fd;
return my_cpc__continuation_96;
}
void (accept_connection (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__handle_connection_arglist {
int  fd ;
}
;
static inline struct cpc_continuation *(my_cpc__handle_connection_new_arglist (int fd , struct cpc_continuation *my_cpc__continuation_98 )){
struct my_cpc__handle_connection_arglist *my_cpc__arglist_99 ;
my_cpc__arglist_99 = cpc_alloc((& my_cpc__continuation_98), sizeof(struct my_cpc__handle_connection_arglist ));
(my_cpc__arglist_99 -> fd) = fd;
return my_cpc__continuation_98;
}
void (handle_connection (struct cpc_continuation *cpc_current_continuation ));
static struct sockaddr_in addr ;
static socklen_t len ;
char *root  = "/usr/share/polipo/www/";
int port  = 8666;
int (main ());
int main (){
int fd , rc ;
int one  = 1;
signal(13, ((__sighandler_t )1));
fd = socket(2, SOCK_STREAM, 0);
if(fd < 0)
{
perror("socket");
exit(1);
}
rc = setsockopt(fd, 1, 2, ((char *)(& one)), sizeof(one));
if(rc < 0)
perror("setsockopt(SO_REUSEADDR)");
memset((& addr), 0, sizeof(addr));
(addr . sin_family) = 2;
(addr . sin_port) = htons(port);
rc = bind(fd, ((struct sockaddr *)(& addr)), sizeof(addr));
if(rc < 0)
{
perror("bind");
exit(1);
}
rc = listen(fd, 1024);
if(rc < 0)
{
perror("listen");
exit(1);
}
rc = cpc_setup_descriptor(fd, 1);
if(rc < 0)
{
perror("setup_descriptor");
exit(1);
}
{
struct cpc_continuation *my_cpc__apply_later_100 ;
my_cpc__apply_later_100 = ((void *)0);
my_cpc__apply_later_100 = my_cpc__accept_connection_new_arglist(fd, my_cpc__apply_later_100);
my_cpc__apply_later_100 = cpc_continuation_push(my_cpc__apply_later_100, ((cpc_function *)accept_connection));
cpc_schedule(my_cpc__apply_later_100);
}
cpc_main_loop();
return 0;
}
void (accept_connection (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__explicit_3_6_arglist {
int  fd ;
int  rc ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__explicit_3_6_new_arglist (int fd , int rc , struct cpc_continuation *my_cpc__continuation_101 )){
struct my_cpc__my_cpc__my_cpc__explicit_3_6_arglist *my_cpc__arglist_102 ;
my_cpc__arglist_102 = cpc_alloc((& my_cpc__continuation_101), sizeof(struct my_cpc__my_cpc__my_cpc__explicit_3_6_arglist ));
(my_cpc__arglist_102 -> fd) = fd;
(my_cpc__arglist_102 -> rc) = rc;
return my_cpc__continuation_101;
}
void (my_cpc__my_cpc__explicit_3_6 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__continue_1_9_arglist {
int  fd ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__continue_1_9_new_arglist (int fd , struct cpc_continuation *my_cpc__continuation_103 )){
struct my_cpc__my_cpc__my_cpc__continue_1_9_arglist *my_cpc__arglist_104 ;
my_cpc__arglist_104 = cpc_alloc((& my_cpc__continuation_103), sizeof(struct my_cpc__my_cpc__my_cpc__continue_1_9_arglist ));
(my_cpc__arglist_104 -> fd) = fd;
return my_cpc__continuation_103;
}
void (my_cpc__my_cpc__continue_1_9 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__trivial_block_5_26_arglist {
int  fd ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__trivial_block_5_26_new_arglist (int fd , struct cpc_continuation *my_cpc__continuation_105 )){
struct my_cpc__my_cpc__my_cpc__trivial_block_5_26_arglist *my_cpc__arglist_106 ;
my_cpc__arglist_106 = cpc_alloc((& my_cpc__continuation_105), sizeof(struct my_cpc__my_cpc__my_cpc__trivial_block_5_26_arglist ));
(my_cpc__arglist_106 -> fd) = fd;
return my_cpc__continuation_105;
}
void (my_cpc__my_cpc__trivial_block_5_26 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__trivial_block_8_29_arglist {
int  fd ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__trivial_block_8_29_new_arglist (int fd , struct cpc_continuation *my_cpc__continuation_107 )){
struct my_cpc__my_cpc__my_cpc__trivial_block_8_29_arglist *my_cpc__arglist_108 ;
my_cpc__arglist_108 = cpc_alloc((& my_cpc__continuation_107), sizeof(struct my_cpc__my_cpc__my_cpc__trivial_block_8_29_arglist ));
(my_cpc__arglist_108 -> fd) = fd;
return my_cpc__continuation_107;
}
void (my_cpc__my_cpc__trivial_block_8_29 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__trivial_if_28_38_arglist {
int  dummy ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__trivial_if_28_38_new_arglist (struct cpc_continuation *my_cpc__continuation_109 )){
struct my_cpc__my_cpc__my_cpc__trivial_if_28_38_arglist *my_cpc__arglist_110 ;
my_cpc__arglist_110 = cpc_alloc((& my_cpc__continuation_109), sizeof(struct my_cpc__my_cpc__my_cpc__trivial_if_28_38_arglist ));
return my_cpc__continuation_109;
}
void (my_cpc__my_cpc__trivial_if_28_38 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__trivial_block_36_37_arglist {
int  dummy ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__trivial_block_36_37_new_arglist (struct cpc_continuation *my_cpc__continuation_111 )){
struct my_cpc__my_cpc__my_cpc__trivial_block_36_37_arglist *my_cpc__arglist_112 ;
my_cpc__arglist_112 = cpc_alloc((& my_cpc__continuation_111), sizeof(struct my_cpc__my_cpc__my_cpc__trivial_block_36_37_arglist ));
return my_cpc__continuation_111;
}
void (my_cpc__my_cpc__trivial_block_36_37 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__while_2_32_arglist {
int  fd ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__while_2_32_new_arglist (int fd , struct cpc_continuation *my_cpc__continuation_113 )){
struct my_cpc__my_cpc__my_cpc__while_2_32_arglist *my_cpc__arglist_114 ;
my_cpc__arglist_114 = cpc_alloc((& my_cpc__continuation_113), sizeof(struct my_cpc__my_cpc__my_cpc__while_2_32_arglist ));
(my_cpc__arglist_114 -> fd) = fd;
return my_cpc__continuation_113;
}
void (my_cpc__my_cpc__while_2_32 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__trivial_block_31_34_arglist {
int  dummy ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__trivial_block_31_34_new_arglist (struct cpc_continuation *my_cpc__continuation_115 )){
struct my_cpc__my_cpc__my_cpc__trivial_block_31_34_arglist *my_cpc__arglist_116 ;
my_cpc__arglist_116 = cpc_alloc((& my_cpc__continuation_115), sizeof(struct my_cpc__my_cpc__my_cpc__trivial_block_31_34_arglist ));
return my_cpc__continuation_115;
}
void (my_cpc__my_cpc__trivial_block_31_34 (struct cpc_continuation *cpc_current_continuation ));
void my_cpc__my_cpc__trivial_block_31_34 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__trivial_block_31_34_arglist *cpc_arguments ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__trivial_block_31_34_arglist ));
;
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__while_2_32 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__while_2_32_arglist *cpc_arguments ;
int fd ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__while_2_32_arglist ));
fd = (cpc_arguments -> fd);
{
if(1)
{
{
{
int rc ;
cpc_current_continuation = my_cpc__my_cpc__my_cpc__explicit_3_6_new_arglist(fd, rc, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__explicit_3_6));
cpc_prim_io_wait(fd, CPC_IO_IN, ((void *)0), cpc_current_continuation);
return;
cpc_invoke_continuation(cpc_current_continuation);
return;
}
cpc_current_continuation = my_cpc__my_cpc__my_cpc__while_2_32_new_arglist(fd, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__while_2_32));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
cpc_current_continuation = my_cpc__my_cpc__my_cpc__while_2_32_new_arglist(fd, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__while_2_32));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_block_36_37_new_arglist(cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_block_36_37));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_block_36_37_new_arglist(cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_block_36_37));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__trivial_block_36_37 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__trivial_block_36_37_arglist *cpc_arguments ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__trivial_block_36_37_arglist ));
my_cpc__break_0: ;
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__trivial_if_28_38 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__trivial_if_28_38_arglist *cpc_arguments ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__trivial_if_28_38_arglist ));
;
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_block_36_37_new_arglist(cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_block_36_37));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__trivial_block_8_29 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__trivial_block_8_29_arglist *cpc_arguments ;
int fd ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__trivial_block_8_29_arglist ));
fd = (cpc_arguments -> fd);
cpc_current_continuation = my_cpc__my_cpc__my_cpc__while_2_32_new_arglist(fd, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__while_2_32));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__trivial_block_5_26 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__trivial_block_5_26_arglist *cpc_arguments ;
int fd ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__trivial_block_5_26_arglist ));
fd = (cpc_arguments -> fd);
;
cpc_current_continuation = my_cpc__my_cpc__my_cpc__while_2_32_new_arglist(fd, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__while_2_32));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__continue_1_9 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__continue_1_9_arglist *cpc_arguments ;
int fd ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__continue_1_9_arglist ));
fd = (cpc_arguments -> fd);
;
cpc_current_continuation = my_cpc__my_cpc__my_cpc__while_2_32_new_arglist(fd, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__while_2_32));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__explicit_3_6 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__explicit_3_6_arglist *cpc_arguments ;
int fd ;
int rc ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__explicit_3_6_arglist ));
fd = (cpc_arguments -> fd);
rc = (cpc_arguments -> rc);
len = sizeof(addr);
rc = accept(fd, ((struct sockaddr *)(& addr)), (& len));
if(rc < 0)
{
if((* __errno_location()) == 4)
{
cpc_current_continuation = my_cpc__my_cpc__my_cpc__while_2_32_new_arglist(fd, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__while_2_32));
cpc_invoke_continuation(cpc_current_continuation);
return;
}

else
{
perror("accept");
cpc_current_continuation = my_cpc__my_cpc__my_cpc__while_2_32_new_arglist(fd, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__while_2_32));
cpc_schedule(cpc_current_continuation);
return;
cpc_invoke_continuation(cpc_current_continuation);
return;
}
}
{
struct cpc_continuation *my_cpc__apply_later_117 ;
my_cpc__apply_later_117 = ((void *)0);
my_cpc__apply_later_117 = my_cpc__handle_connection_new_arglist(rc, my_cpc__apply_later_117);
my_cpc__apply_later_117 = cpc_continuation_push(my_cpc__apply_later_117, ((cpc_function *)handle_connection));
cpc_schedule(my_cpc__apply_later_117);
}
cpc_current_continuation = my_cpc__my_cpc__my_cpc__while_2_32_new_arglist(fd, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__while_2_32));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void accept_connection (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__accept_connection_arglist *cpc_arguments ;
int fd ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__accept_connection_arglist ));
fd = (cpc_arguments -> fd);
{
cpc_current_continuation = my_cpc__my_cpc__my_cpc__while_2_32_new_arglist(fd, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__while_2_32));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void (handle_connection (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__explicit_12_13_arglist {
int  i ;
int  tmp ;
int  ffd ;
char  *fn ;
int  rc ;
int  fd ;
char  *buf ;
int  n ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__explicit_12_13_new_arglist (int i , int tmp , int ffd , char *fn , int rc , int fd , char *buf , int n , struct cpc_continuation *my_cpc__continuation_118 )){
struct my_cpc__my_cpc__my_cpc__explicit_12_13_arglist *my_cpc__arglist_119 ;
my_cpc__arglist_119 = cpc_alloc((& my_cpc__continuation_118), sizeof(struct my_cpc__my_cpc__my_cpc__explicit_12_13_arglist ));
(my_cpc__arglist_119 -> i) = i;
(my_cpc__arglist_119 -> tmp) = tmp;
(my_cpc__arglist_119 -> ffd) = ffd;
(my_cpc__arglist_119 -> fn) = fn;
(my_cpc__arglist_119 -> rc) = rc;
(my_cpc__arglist_119 -> fd) = fd;
(my_cpc__arglist_119 -> buf) = buf;
(my_cpc__arglist_119 -> n) = n;
return my_cpc__continuation_118;
}
void (my_cpc__my_cpc__explicit_12_13 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__send_54_arglist {
int  ffd ;
char  *fn ;
int  rc ;
int  fd ;
int  n ;
char  *buf ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__send_54_new_arglist (int ffd , char *fn , int rc , int fd , int n , char *buf , struct cpc_continuation *my_cpc__continuation_120 )){
struct my_cpc__my_cpc__send_54_arglist *my_cpc__arglist_121 ;
my_cpc__arglist_121 = cpc_alloc((& my_cpc__continuation_120), sizeof(struct my_cpc__my_cpc__send_54_arglist ));
(my_cpc__arglist_121 -> ffd) = ffd;
(my_cpc__arglist_121 -> fn) = fn;
(my_cpc__arglist_121 -> rc) = rc;
(my_cpc__arglist_121 -> fd) = fd;
(my_cpc__arglist_121 -> n) = n;
(my_cpc__arglist_121 -> buf) = buf;
return my_cpc__continuation_120;
}
void (my_cpc__send_54 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__search_53_arglist {
int  i ;
int  tmp ;
int  ffd ;
char  *fn ;
int  rc ;
int  fd ;
int  n ;
char  *buf ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__search_53_new_arglist (int i , int tmp , int ffd , char *fn , int rc , int fd , int n , char *buf , struct cpc_continuation *my_cpc__continuation_122 )){
struct my_cpc__my_cpc__search_53_arglist *my_cpc__arglist_123 ;
my_cpc__arglist_123 = cpc_alloc((& my_cpc__continuation_122), sizeof(struct my_cpc__my_cpc__search_53_arglist ));
(my_cpc__arglist_123 -> i) = i;
(my_cpc__arglist_123 -> tmp) = tmp;
(my_cpc__arglist_123 -> ffd) = ffd;
(my_cpc__arglist_123 -> fn) = fn;
(my_cpc__arglist_123 -> rc) = rc;
(my_cpc__arglist_123 -> fd) = fd;
(my_cpc__arglist_123 -> n) = n;
(my_cpc__arglist_123 -> buf) = buf;
return my_cpc__continuation_122;
}
void (my_cpc__search_53 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__explicit_19_22_arglist {
int  rc ;
char  *buf ;
int  fd ;
int  ffd ;
int  n ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__explicit_19_22_new_arglist (int rc , char *buf , int fd , int ffd , int n , struct cpc_continuation *my_cpc__continuation_124 )){
struct my_cpc__my_cpc__my_cpc__explicit_19_22_arglist *my_cpc__arglist_125 ;
my_cpc__arglist_125 = cpc_alloc((& my_cpc__continuation_124), sizeof(struct my_cpc__my_cpc__my_cpc__explicit_19_22_arglist ));
(my_cpc__arglist_125 -> rc) = rc;
(my_cpc__arglist_125 -> buf) = buf;
(my_cpc__arglist_125 -> fd) = fd;
(my_cpc__arglist_125 -> ffd) = ffd;
(my_cpc__arglist_125 -> n) = n;
return my_cpc__continuation_124;
}
void (my_cpc__my_cpc__explicit_19_22 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__trivial_block_21_25_arglist {
int  rc ;
char  *buf ;
int  fd ;
int  n ;
int  ffd ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__trivial_block_21_25_new_arglist (int rc , char *buf , int fd , int n , int ffd , struct cpc_continuation *my_cpc__continuation_126 )){
struct my_cpc__my_cpc__my_cpc__trivial_block_21_25_arglist *my_cpc__arglist_127 ;
my_cpc__arglist_127 = cpc_alloc((& my_cpc__continuation_126), sizeof(struct my_cpc__my_cpc__my_cpc__trivial_block_21_25_arglist ));
(my_cpc__arglist_127 -> rc) = rc;
(my_cpc__arglist_127 -> buf) = buf;
(my_cpc__arglist_127 -> fd) = fd;
(my_cpc__arglist_127 -> n) = n;
(my_cpc__arglist_127 -> ffd) = ffd;
return my_cpc__continuation_126;
}
void (my_cpc__my_cpc__trivial_block_21_25 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__trivial_block_24_50_arglist {
int  rc ;
char  *buf ;
int  fd ;
int  ffd ;
int  n ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__trivial_block_24_50_new_arglist (int rc , char *buf , int fd , int ffd , int n , struct cpc_continuation *my_cpc__continuation_128 )){
struct my_cpc__my_cpc__my_cpc__trivial_block_24_50_arglist *my_cpc__arglist_129 ;
my_cpc__arglist_129 = cpc_alloc((& my_cpc__continuation_128), sizeof(struct my_cpc__my_cpc__my_cpc__trivial_block_24_50_arglist ));
(my_cpc__arglist_129 -> rc) = rc;
(my_cpc__arglist_129 -> buf) = buf;
(my_cpc__arglist_129 -> fd) = fd;
(my_cpc__arglist_129 -> ffd) = ffd;
(my_cpc__arglist_129 -> n) = n;
return my_cpc__continuation_128;
}
void (my_cpc__my_cpc__trivial_block_24_50 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__break_16_51_arglist {
int  ffd ;
int  fd ;
char  *buf ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__break_16_51_new_arglist (int ffd , int fd , char *buf , struct cpc_continuation *my_cpc__continuation_130 )){
struct my_cpc__my_cpc__my_cpc__break_16_51_arglist *my_cpc__arglist_131 ;
my_cpc__arglist_131 = cpc_alloc((& my_cpc__continuation_130), sizeof(struct my_cpc__my_cpc__my_cpc__break_16_51_arglist ));
(my_cpc__arglist_131 -> ffd) = ffd;
(my_cpc__arglist_131 -> fd) = fd;
(my_cpc__arglist_131 -> buf) = buf;
return my_cpc__continuation_130;
}
void (my_cpc__my_cpc__break_16_51 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__trivial_if_48_49_arglist {
int  ffd ;
int  fd ;
char  *buf ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__trivial_if_48_49_new_arglist (int ffd , int fd , char *buf , struct cpc_continuation *my_cpc__continuation_132 )){
struct my_cpc__my_cpc__my_cpc__trivial_if_48_49_arglist *my_cpc__arglist_133 ;
my_cpc__arglist_133 = cpc_alloc((& my_cpc__continuation_132), sizeof(struct my_cpc__my_cpc__my_cpc__trivial_if_48_49_arglist ));
(my_cpc__arglist_133 -> ffd) = ffd;
(my_cpc__arglist_133 -> fd) = fd;
(my_cpc__arglist_133 -> buf) = buf;
return my_cpc__continuation_132;
}
void (my_cpc__my_cpc__trivial_if_48_49 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__while_18_46_arglist {
int  rc ;
char  *buf ;
int  fd ;
int  n ;
int  ffd ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__while_18_46_new_arglist (int rc , char *buf , int fd , int n , int ffd , struct cpc_continuation *my_cpc__continuation_134 )){
struct my_cpc__my_cpc__my_cpc__while_18_46_arglist *my_cpc__arglist_135 ;
my_cpc__arglist_135 = cpc_alloc((& my_cpc__continuation_134), sizeof(struct my_cpc__my_cpc__my_cpc__while_18_46_arglist ));
(my_cpc__arglist_135 -> rc) = rc;
(my_cpc__arglist_135 -> buf) = buf;
(my_cpc__arglist_135 -> fd) = fd;
(my_cpc__arglist_135 -> n) = n;
(my_cpc__arglist_135 -> ffd) = ffd;
return my_cpc__continuation_134;
}
void (my_cpc__my_cpc__while_18_46 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__trivial_block_41_45_arglist {
int  ffd ;
char  *buf ;
int  fd ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__trivial_block_41_45_new_arglist (int ffd , char *buf , int fd , struct cpc_continuation *my_cpc__continuation_136 )){
struct my_cpc__my_cpc__my_cpc__trivial_block_41_45_arglist *my_cpc__arglist_137 ;
my_cpc__arglist_137 = cpc_alloc((& my_cpc__continuation_136), sizeof(struct my_cpc__my_cpc__my_cpc__trivial_block_41_45_arglist ));
(my_cpc__arglist_137 -> ffd) = ffd;
(my_cpc__arglist_137 -> buf) = buf;
(my_cpc__arglist_137 -> fd) = fd;
return my_cpc__continuation_136;
}
void (my_cpc__my_cpc__trivial_block_41_45 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__fail_52_arglist {
int  fd ;
char  *buf ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__fail_52_new_arglist (int fd , char *buf , struct cpc_continuation *my_cpc__continuation_138 )){
struct my_cpc__my_cpc__fail_52_arglist *my_cpc__arglist_139 ;
my_cpc__arglist_139 = cpc_alloc((& my_cpc__continuation_138), sizeof(struct my_cpc__my_cpc__fail_52_arglist ));
(my_cpc__arglist_139 -> fd) = fd;
(my_cpc__arglist_139 -> buf) = buf;
return my_cpc__continuation_138;
}
void (my_cpc__fail_52 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__trivial_if_43_44_arglist {
int  ffd ;
int  fd ;
char  *buf ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__trivial_if_43_44_new_arglist (int ffd , int fd , char *buf , struct cpc_continuation *my_cpc__continuation_140 )){
struct my_cpc__my_cpc__my_cpc__trivial_if_43_44_arglist *my_cpc__arglist_141 ;
my_cpc__arglist_141 = cpc_alloc((& my_cpc__continuation_140), sizeof(struct my_cpc__my_cpc__my_cpc__trivial_if_43_44_arglist ));
(my_cpc__arglist_141 -> ffd) = ffd;
(my_cpc__arglist_141 -> fd) = fd;
(my_cpc__arglist_141 -> buf) = buf;
return my_cpc__continuation_140;
}
void (my_cpc__my_cpc__trivial_if_43_44 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__explicit_14_15_arglist {
int  rc ;
int  n ;
int  ffd ;
int  fd ;
char  *buf ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__explicit_14_15_new_arglist (int rc , int n , int ffd , int fd , char *buf , struct cpc_continuation *my_cpc__continuation_142 )){
struct my_cpc__my_cpc__my_cpc__explicit_14_15_arglist *my_cpc__arglist_143 ;
my_cpc__arglist_143 = cpc_alloc((& my_cpc__continuation_142), sizeof(struct my_cpc__my_cpc__my_cpc__explicit_14_15_arglist ));
(my_cpc__arglist_143 -> rc) = rc;
(my_cpc__arglist_143 -> n) = n;
(my_cpc__arglist_143 -> ffd) = ffd;
(my_cpc__arglist_143 -> fd) = fd;
(my_cpc__arglist_143 -> buf) = buf;
return my_cpc__continuation_142;
}
void (my_cpc__my_cpc__explicit_14_15 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__my_cpc__trivial_label_10_11_arglist {
int  len ;
char  *fn ;
int  tmp ;
int  i ;
int  rc ;
int  n ;
int  ffd ;
int  fd ;
char  *buf ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__my_cpc__trivial_label_10_11_new_arglist (int len , char *fn , int tmp , int i , int rc , int n , int ffd , int fd , char *buf , struct cpc_continuation *my_cpc__continuation_144 )){
struct my_cpc__my_cpc__my_cpc__trivial_label_10_11_arglist *my_cpc__arglist_145 ;
my_cpc__arglist_145 = cpc_alloc((& my_cpc__continuation_144), sizeof(struct my_cpc__my_cpc__my_cpc__trivial_label_10_11_arglist ));
(my_cpc__arglist_145 -> len) = len;
(my_cpc__arglist_145 -> fn) = fn;
(my_cpc__arglist_145 -> tmp) = tmp;
(my_cpc__arglist_145 -> i) = i;
(my_cpc__arglist_145 -> rc) = rc;
(my_cpc__arglist_145 -> n) = n;
(my_cpc__arglist_145 -> ffd) = ffd;
(my_cpc__arglist_145 -> fd) = fd;
(my_cpc__arglist_145 -> buf) = buf;
return my_cpc__continuation_144;
}
void (my_cpc__my_cpc__trivial_label_10_11 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__again_39_arglist {
int  len ;
char  *fn ;
int  tmp ;
int  i ;
int  rc ;
int  n ;
int  ffd ;
int  fd ;
char  *buf ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__again_39_new_arglist (int len , char *fn , int tmp , int i , int rc , int n , int ffd , int fd , char *buf , struct cpc_continuation *my_cpc__continuation_146 )){
struct my_cpc__my_cpc__again_39_arglist *my_cpc__arglist_147 ;
my_cpc__arglist_147 = cpc_alloc((& my_cpc__continuation_146), sizeof(struct my_cpc__my_cpc__again_39_arglist ));
(my_cpc__arglist_147 -> len) = len;
(my_cpc__arglist_147 -> fn) = fn;
(my_cpc__arglist_147 -> tmp) = tmp;
(my_cpc__arglist_147 -> i) = i;
(my_cpc__arglist_147 -> rc) = rc;
(my_cpc__arglist_147 -> n) = n;
(my_cpc__arglist_147 -> ffd) = ffd;
(my_cpc__arglist_147 -> fd) = fd;
(my_cpc__arglist_147 -> buf) = buf;
return my_cpc__continuation_146;
}
void (my_cpc__again_39 (struct cpc_continuation *cpc_current_continuation ));
struct my_cpc__my_cpc__linear_my_cpc__my_cpc__trivial_label_10_11_tmp_64_arglist {
int  my_cpc__len_55 ;
char  *my_cpc__fn_56 ;
int  my_cpc__i_58 ;
int  my_cpc__rc_59 ;
int  my_cpc__n_60 ;
int  my_cpc__ffd_61 ;
int  my_cpc__fd_62 ;
char  *my_cpc__buf_63 ;
int  my_cpc__tmp_57 ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__linear_my_cpc__my_cpc__trivial_label_10_11_tmp_64_new_arglist (int my_cpc__len_55 , char *my_cpc__fn_56 , int my_cpc__i_58 , int my_cpc__rc_59 , int my_cpc__n_60 , int my_cpc__ffd_61 , int my_cpc__fd_62 , char *my_cpc__buf_63 , int my_cpc__tmp_57 , struct cpc_continuation *my_cpc__continuation_148 )){
struct my_cpc__my_cpc__linear_my_cpc__my_cpc__trivial_label_10_11_tmp_64_arglist *my_cpc__arglist_149 ;
my_cpc__arglist_149 = cpc_alloc((& my_cpc__continuation_148), sizeof(struct my_cpc__my_cpc__linear_my_cpc__my_cpc__trivial_label_10_11_tmp_64_arglist ));
(my_cpc__arglist_149 -> my_cpc__len_55) = my_cpc__len_55;
(my_cpc__arglist_149 -> my_cpc__fn_56) = my_cpc__fn_56;
(my_cpc__arglist_149 -> my_cpc__i_58) = my_cpc__i_58;
(my_cpc__arglist_149 -> my_cpc__rc_59) = my_cpc__rc_59;
(my_cpc__arglist_149 -> my_cpc__n_60) = my_cpc__n_60;
(my_cpc__arglist_149 -> my_cpc__ffd_61) = my_cpc__ffd_61;
(my_cpc__arglist_149 -> my_cpc__fd_62) = my_cpc__fd_62;
(my_cpc__arglist_149 -> my_cpc__buf_63) = my_cpc__buf_63;
(my_cpc__arglist_149 -> my_cpc__tmp_57) = my_cpc__tmp_57;
return my_cpc__continuation_148;
}
void (my_cpc__linear_my_cpc__my_cpc__trivial_label_10_11_tmp_64 (struct cpc_continuation *cpc_current_continuation ));
void my_cpc__again_39 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__again_39_arglist *cpc_arguments ;
int len ;
char *fn ;
int tmp ;
int i ;
int rc ;
int n ;
int ffd ;
int fd ;
char *buf ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__again_39_arglist ));
len = (cpc_arguments -> len);
fn = (cpc_arguments -> fn);
tmp = (cpc_arguments -> tmp);
i = (cpc_arguments -> i);
rc = (cpc_arguments -> rc);
n = (cpc_arguments -> n);
ffd = (cpc_arguments -> ffd);
fd = (cpc_arguments -> fd);
buf = (cpc_arguments -> buf);
{
cpc_current_continuation = my_cpc__my_cpc__linear_my_cpc__my_cpc__trivial_label_10_11_tmp_64_new_arglist(len, fn, i, rc, n, ffd, fd, buf, tmp, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__linear_my_cpc__my_cpc__trivial_label_10_11_tmp_64));
cpc_current_continuation = my_cpc__cpc_read_timeout_new_arglist(fd, (buf + rc), (4096 - rc), 60, 0, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)cpc_read_timeout));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_label_10_11_new_arglist(len, fn, tmp, i, rc, n, ffd, fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_label_10_11));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__linear_my_cpc__my_cpc__trivial_label_10_11_tmp_64 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__linear_my_cpc__my_cpc__trivial_label_10_11_tmp_64_arglist *cpc_arguments ;
int my_cpc__len_55 ;
char *my_cpc__fn_56 ;
int my_cpc__i_58 ;
int my_cpc__rc_59 ;
int my_cpc__n_60 ;
int my_cpc__ffd_61 ;
int my_cpc__fd_62 ;
char *my_cpc__buf_63 ;
int my_cpc__tmp_57 ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__linear_my_cpc__my_cpc__trivial_label_10_11_tmp_64_arglist ));
my_cpc__len_55 = (cpc_arguments -> my_cpc__len_55);
my_cpc__fn_56 = (cpc_arguments -> my_cpc__fn_56);
my_cpc__i_58 = (cpc_arguments -> my_cpc__i_58);
my_cpc__rc_59 = (cpc_arguments -> my_cpc__rc_59);
my_cpc__n_60 = (cpc_arguments -> my_cpc__n_60);
my_cpc__ffd_61 = (cpc_arguments -> my_cpc__ffd_61);
my_cpc__fd_62 = (cpc_arguments -> my_cpc__fd_62);
my_cpc__buf_63 = (cpc_arguments -> my_cpc__buf_63);
my_cpc__tmp_57 = (cpc_arguments -> my_cpc__tmp_57);
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_label_10_11_new_arglist(my_cpc__len_55, my_cpc__fn_56, my_cpc__tmp_57, my_cpc__i_58, my_cpc__rc_59, my_cpc__n_60, my_cpc__ffd_61, my_cpc__fd_62, my_cpc__buf_63, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_label_10_11));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__trivial_label_10_11 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__trivial_label_10_11_arglist *cpc_arguments ;
int len ;
char *fn ;
int tmp ;
int i ;
int rc ;
int n ;
int ffd ;
int fd ;
char *buf ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__trivial_label_10_11_arglist ));
len = (cpc_arguments -> len);
fn = (cpc_arguments -> fn);
tmp = (cpc_arguments -> tmp);
i = (cpc_arguments -> i);
rc = (cpc_arguments -> rc);
n = (cpc_arguments -> n);
ffd = (cpc_arguments -> ffd);
fd = (cpc_arguments -> fd);
buf = (cpc_arguments -> buf);
rc += tmp;
if(rc < 0)
{
perror("read");
cpc_current_continuation = my_cpc__my_cpc__fail_52_new_arglist(fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__fail_52));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
if(rc < 4)
{
cpc_current_continuation = my_cpc__my_cpc__fail_52_new_arglist(fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__fail_52));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
if(memcmp(buf, "GET ", 4) != 0)
{
cpc_current_continuation = my_cpc__my_cpc__fail_52_new_arglist(fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__fail_52));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
for(i = 5; i < rc; i++) if((((buf[i]) == ' ') || ((buf[i]) == '\r')) || ((buf[i]) == '\n'))
break;
if((i == rc) && (rc < 4096))
{
cpc_current_continuation = my_cpc__my_cpc__again_39_new_arglist(len, fn, tmp, i, rc, n, ffd, fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__again_39));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
len = strlen(root);
fn = malloc(((((len + 1) + i) - 5) + 12));
strcpy(fn, root);
memcpy((fn + len), (buf + 5), (i - 5));
if((buf[i - 1]) == '/')
strcpy((((fn + len) + i) - 5), "index.html");

else
(fn[(len + i) - 5]) = '\0';
i--;
cpc_current_continuation = my_cpc__my_cpc__search_53_new_arglist(i, tmp, ffd, fn, rc, fd, n, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__search_53));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__explicit_14_15 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__explicit_14_15_arglist *cpc_arguments ;
int rc ;
int n ;
int ffd ;
int fd ;
char *buf ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__explicit_14_15_arglist ));
rc = (cpc_arguments -> rc);
n = (cpc_arguments -> n);
ffd = (cpc_arguments -> ffd);
fd = (cpc_arguments -> fd);
buf = (cpc_arguments -> buf);
if(rc < 0)
{
perror("write");
if(ffd >= 0)
close(ffd);
cpc_current_continuation = my_cpc__my_cpc__fail_52_new_arglist(fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__fail_52));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
if(ffd >= 0)
{
{
cpc_current_continuation = my_cpc__my_cpc__my_cpc__while_18_46_new_arglist(rc, buf, fd, n, ffd, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__while_18_46));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_if_43_44_new_arglist(ffd, fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_if_43_44));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_if_43_44_new_arglist(ffd, fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_if_43_44));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__trivial_if_43_44 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__trivial_if_43_44_arglist *cpc_arguments ;
int ffd ;
int fd ;
char *buf ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__trivial_if_43_44_arglist ));
ffd = (cpc_arguments -> ffd);
fd = (cpc_arguments -> fd);
buf = (cpc_arguments -> buf);
close(ffd);
cpc_current_continuation = my_cpc__my_cpc__fail_52_new_arglist(fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__fail_52));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__fail_52 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__fail_52_arglist *cpc_arguments ;
int fd ;
char *buf ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__fail_52_arglist ));
fd = (cpc_arguments -> fd);
buf = (cpc_arguments -> buf);
close(fd);
free(buf);
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__trivial_block_41_45 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__trivial_block_41_45_arglist *cpc_arguments ;
int ffd ;
char *buf ;
int fd ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__trivial_block_41_45_arglist ));
ffd = (cpc_arguments -> ffd);
buf = (cpc_arguments -> buf);
fd = (cpc_arguments -> fd);
;
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_if_43_44_new_arglist(ffd, fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_if_43_44));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
struct my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_19_22_rc_70_arglist {
char  *my_cpc__buf_66 ;
int  my_cpc__fd_67 ;
int  my_cpc__ffd_68 ;
int  my_cpc__n_69 ;
int  my_cpc__rc_65 ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_19_22_rc_70_new_arglist (char *my_cpc__buf_66 , int my_cpc__fd_67 , int my_cpc__ffd_68 , int my_cpc__n_69 , int my_cpc__rc_65 , struct cpc_continuation *my_cpc__continuation_150 )){
struct my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_19_22_rc_70_arglist *my_cpc__arglist_151 ;
my_cpc__arglist_151 = cpc_alloc((& my_cpc__continuation_150), sizeof(struct my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_19_22_rc_70_arglist ));
(my_cpc__arglist_151 -> my_cpc__buf_66) = my_cpc__buf_66;
(my_cpc__arglist_151 -> my_cpc__fd_67) = my_cpc__fd_67;
(my_cpc__arglist_151 -> my_cpc__ffd_68) = my_cpc__ffd_68;
(my_cpc__arglist_151 -> my_cpc__n_69) = my_cpc__n_69;
(my_cpc__arglist_151 -> my_cpc__rc_65) = my_cpc__rc_65;
return my_cpc__continuation_150;
}
void (my_cpc__linear_my_cpc__my_cpc__explicit_19_22_rc_70 (struct cpc_continuation *cpc_current_continuation ));
void my_cpc__my_cpc__while_18_46 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__while_18_46_arglist *cpc_arguments ;
int rc ;
char *buf ;
int fd ;
int n ;
int ffd ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__while_18_46_arglist ));
rc = (cpc_arguments -> rc);
buf = (cpc_arguments -> buf);
fd = (cpc_arguments -> fd);
n = (cpc_arguments -> n);
ffd = (cpc_arguments -> ffd);
if(1)
{
{
{
n = read(ffd, buf, 4096);
if(n <= 0)
{
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_if_43_44_new_arglist(ffd, fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_if_43_44));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
cpc_current_continuation = my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_19_22_rc_70_new_arglist(buf, fd, ffd, n, rc, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__linear_my_cpc__my_cpc__explicit_19_22_rc_70));
cpc_current_continuation = my_cpc__cpc_write_timeout_new_arglist(fd, buf, 4096, 60, 0, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)cpc_write_timeout));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_block_21_25_new_arglist(rc, buf, fd, n, ffd, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_block_21_25));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_block_24_50_new_arglist(rc, buf, fd, ffd, n, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_block_24_50));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_if_43_44_new_arglist(ffd, fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_if_43_44));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__linear_my_cpc__my_cpc__explicit_19_22_rc_70 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_19_22_rc_70_arglist *cpc_arguments ;
char *my_cpc__buf_66 ;
int my_cpc__fd_67 ;
int my_cpc__ffd_68 ;
int my_cpc__n_69 ;
int my_cpc__rc_65 ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_19_22_rc_70_arglist ));
my_cpc__buf_66 = (cpc_arguments -> my_cpc__buf_66);
my_cpc__fd_67 = (cpc_arguments -> my_cpc__fd_67);
my_cpc__ffd_68 = (cpc_arguments -> my_cpc__ffd_68);
my_cpc__n_69 = (cpc_arguments -> my_cpc__n_69);
my_cpc__rc_65 = (cpc_arguments -> my_cpc__rc_65);
cpc_current_continuation = my_cpc__my_cpc__my_cpc__explicit_19_22_new_arglist(my_cpc__rc_65, my_cpc__buf_66, my_cpc__fd_67, my_cpc__ffd_68, my_cpc__n_69, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__explicit_19_22));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__trivial_if_48_49 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__trivial_if_48_49_arglist *cpc_arguments ;
int ffd ;
int fd ;
char *buf ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__trivial_if_48_49_arglist ));
ffd = (cpc_arguments -> ffd);
fd = (cpc_arguments -> fd);
buf = (cpc_arguments -> buf);
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_if_43_44_new_arglist(ffd, fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_if_43_44));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__break_16_51 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__break_16_51_arglist *cpc_arguments ;
int ffd ;
int fd ;
char *buf ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__break_16_51_arglist ));
ffd = (cpc_arguments -> ffd);
fd = (cpc_arguments -> fd);
buf = (cpc_arguments -> buf);
;
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_if_43_44_new_arglist(ffd, fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_if_43_44));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__trivial_block_24_50 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__trivial_block_24_50_arglist *cpc_arguments ;
int rc ;
char *buf ;
int fd ;
int ffd ;
int n ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__trivial_block_24_50_arglist ));
rc = (cpc_arguments -> rc);
buf = (cpc_arguments -> buf);
fd = (cpc_arguments -> fd);
ffd = (cpc_arguments -> ffd);
n = (cpc_arguments -> n);
{
cpc_current_continuation = my_cpc__my_cpc__my_cpc__while_18_46_new_arglist(rc, buf, fd, n, ffd, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__while_18_46));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_if_43_44_new_arglist(ffd, fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_if_43_44));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__trivial_block_21_25 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__trivial_block_21_25_arglist *cpc_arguments ;
int rc ;
char *buf ;
int fd ;
int n ;
int ffd ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__trivial_block_21_25_arglist ));
rc = (cpc_arguments -> rc);
buf = (cpc_arguments -> buf);
fd = (cpc_arguments -> fd);
n = (cpc_arguments -> n);
ffd = (cpc_arguments -> ffd);
;
my_cpc__continue_17: ;
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_block_24_50_new_arglist(rc, buf, fd, ffd, n, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_block_24_50));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__explicit_19_22 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__explicit_19_22_arglist *cpc_arguments ;
int rc ;
char *buf ;
int fd ;
int ffd ;
int n ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__explicit_19_22_arglist ));
rc = (cpc_arguments -> rc);
buf = (cpc_arguments -> buf);
fd = (cpc_arguments -> fd);
ffd = (cpc_arguments -> ffd);
n = (cpc_arguments -> n);
cpc_current_continuation = my_cpc__my_cpc__my_cpc__trivial_block_21_25_new_arglist(rc, buf, fd, n, ffd, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__trivial_block_21_25));
cpc_schedule(cpc_current_continuation);
return;
cpc_invoke_continuation(cpc_current_continuation);
return;
}
struct my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_12_13_tmp_79_arglist {
int  my_cpc__i_71 ;
int  my_cpc__ffd_73 ;
char  *my_cpc__fn_74 ;
int  my_cpc__rc_75 ;
int  my_cpc__fd_76 ;
char  *my_cpc__buf_77 ;
int  my_cpc__n_78 ;
int  my_cpc__tmp_72 ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_12_13_tmp_79_new_arglist (int my_cpc__i_71 , int my_cpc__ffd_73 , char *my_cpc__fn_74 , int my_cpc__rc_75 , int my_cpc__fd_76 , char *my_cpc__buf_77 , int my_cpc__n_78 , int my_cpc__tmp_72 , struct cpc_continuation *my_cpc__continuation_152 )){
struct my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_12_13_tmp_79_arglist *my_cpc__arglist_153 ;
my_cpc__arglist_153 = cpc_alloc((& my_cpc__continuation_152), sizeof(struct my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_12_13_tmp_79_arglist ));
(my_cpc__arglist_153 -> my_cpc__i_71) = my_cpc__i_71;
(my_cpc__arglist_153 -> my_cpc__ffd_73) = my_cpc__ffd_73;
(my_cpc__arglist_153 -> my_cpc__fn_74) = my_cpc__fn_74;
(my_cpc__arglist_153 -> my_cpc__rc_75) = my_cpc__rc_75;
(my_cpc__arglist_153 -> my_cpc__fd_76) = my_cpc__fd_76;
(my_cpc__arglist_153 -> my_cpc__buf_77) = my_cpc__buf_77;
(my_cpc__arglist_153 -> my_cpc__n_78) = my_cpc__n_78;
(my_cpc__arglist_153 -> my_cpc__tmp_72) = my_cpc__tmp_72;
return my_cpc__continuation_152;
}
void (my_cpc__linear_my_cpc__my_cpc__explicit_12_13_tmp_79 (struct cpc_continuation *cpc_current_continuation ));
void my_cpc__search_53 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__search_53_arglist *cpc_arguments ;
int i ;
int tmp ;
int ffd ;
char *fn ;
int rc ;
int fd ;
int n ;
char *buf ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__search_53_arglist ));
i = (cpc_arguments -> i);
tmp = (cpc_arguments -> tmp);
ffd = (cpc_arguments -> ffd);
fn = (cpc_arguments -> fn);
rc = (cpc_arguments -> rc);
fd = (cpc_arguments -> fd);
n = (cpc_arguments -> n);
buf = (cpc_arguments -> buf);
while(i < (rc - 3))
if(((buf[i++]) == '\r') && ((buf[i]) == '\n'))
{
i++;
if(((buf[i++]) == '\r') && ((buf[i]) == '\n'))
{
cpc_current_continuation = my_cpc__my_cpc__send_54_new_arglist(ffd, fn, rc, fd, n, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__send_54));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
}
if(rc < 4096)
{
cpc_current_continuation = my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_12_13_tmp_79_new_arglist(i, ffd, fn, rc, fd, buf, n, tmp, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__linear_my_cpc__my_cpc__explicit_12_13_tmp_79));
cpc_current_continuation = my_cpc__cpc_read_timeout_new_arglist(fd, (buf + rc), (4096 - rc), 60, 0, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)cpc_read_timeout));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
cpc_current_continuation = my_cpc__my_cpc__send_54_new_arglist(ffd, fn, rc, fd, n, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__send_54));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__linear_my_cpc__my_cpc__explicit_12_13_tmp_79 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_12_13_tmp_79_arglist *cpc_arguments ;
int my_cpc__i_71 ;
int my_cpc__ffd_73 ;
char *my_cpc__fn_74 ;
int my_cpc__rc_75 ;
int my_cpc__fd_76 ;
char *my_cpc__buf_77 ;
int my_cpc__n_78 ;
int my_cpc__tmp_72 ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_12_13_tmp_79_arglist ));
my_cpc__i_71 = (cpc_arguments -> my_cpc__i_71);
my_cpc__ffd_73 = (cpc_arguments -> my_cpc__ffd_73);
my_cpc__fn_74 = (cpc_arguments -> my_cpc__fn_74);
my_cpc__rc_75 = (cpc_arguments -> my_cpc__rc_75);
my_cpc__fd_76 = (cpc_arguments -> my_cpc__fd_76);
my_cpc__buf_77 = (cpc_arguments -> my_cpc__buf_77);
my_cpc__n_78 = (cpc_arguments -> my_cpc__n_78);
my_cpc__tmp_72 = (cpc_arguments -> my_cpc__tmp_72);
cpc_current_continuation = my_cpc__my_cpc__my_cpc__explicit_12_13_new_arglist(my_cpc__i_71, my_cpc__tmp_72, my_cpc__ffd_73, my_cpc__fn_74, my_cpc__rc_75, my_cpc__fd_76, my_cpc__buf_77, my_cpc__n_78, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__explicit_12_13));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
struct my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_14_15_rc_85_arglist {
int  my_cpc__n_81 ;
int  my_cpc__ffd_82 ;
int  my_cpc__fd_83 ;
char  *my_cpc__buf_84 ;
int  my_cpc__rc_80 ;
}
;
static inline struct cpc_continuation *(my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_14_15_rc_85_new_arglist (int my_cpc__n_81 , int my_cpc__ffd_82 , int my_cpc__fd_83 , char *my_cpc__buf_84 , int my_cpc__rc_80 , struct cpc_continuation *my_cpc__continuation_154 )){
struct my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_14_15_rc_85_arglist *my_cpc__arglist_155 ;
my_cpc__arglist_155 = cpc_alloc((& my_cpc__continuation_154), sizeof(struct my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_14_15_rc_85_arglist ));
(my_cpc__arglist_155 -> my_cpc__n_81) = my_cpc__n_81;
(my_cpc__arglist_155 -> my_cpc__ffd_82) = my_cpc__ffd_82;
(my_cpc__arglist_155 -> my_cpc__fd_83) = my_cpc__fd_83;
(my_cpc__arglist_155 -> my_cpc__buf_84) = my_cpc__buf_84;
(my_cpc__arglist_155 -> my_cpc__rc_80) = my_cpc__rc_80;
return my_cpc__continuation_154;
}
void (my_cpc__linear_my_cpc__my_cpc__explicit_14_15_rc_85 (struct cpc_continuation *cpc_current_continuation ));
void my_cpc__send_54 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__send_54_arglist *cpc_arguments ;
int ffd ;
char *fn ;
int rc ;
int fd ;
int n ;
char *buf ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__send_54_arglist ));
ffd = (cpc_arguments -> ffd);
fn = (cpc_arguments -> fn);
rc = (cpc_arguments -> rc);
fd = (cpc_arguments -> fd);
n = (cpc_arguments -> n);
buf = (cpc_arguments -> buf);
ffd = open(fn, 0);
if(ffd < 0)
{
int err ;
char *message ;
if((* __errno_location()) == 2)
{
err = 404;
message = "File doesn't exist";
}

else
if(((* __errno_location()) == 13) || ((* __errno_location()) == 1))
{
err = 403;
message = "Forbidden";
}

else
if(((* __errno_location()) == 24) || ((* __errno_location()) == 23))
{
err = 500;
message = "Out of file descriptors";
}

else
if((* __errno_location()) == 12)
{
err = 500;
message = "Out of memory";
}

else
{
err = 500;
message = "Unknown error";
}
n = snprintf(buf, 4096, "HTTP/1.1 %d %s\r\nContent-Type: text/html\r\nServer: Trivial-cpc\r\nConnection: close\r\n\r\n<html><body><p>Couldn't open %s: %s</body></html>\r\n", err, message, fn, message);
free(fn);
}

else
{
free(fn);
n = snprintf(buf, 4096, "HTTP/1.1 200 OK\r\nContent-Type: text/html\r\nServer: Trivial-cpc\r\nConnection: close\r\n\r\n");
rc = read(ffd, (buf + n), (4096 - n));
if(rc >= 0)
n += rc;
}
cpc_current_continuation = my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_14_15_rc_85_new_arglist(n, ffd, fd, buf, rc, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__linear_my_cpc__my_cpc__explicit_14_15_rc_85));
cpc_current_continuation = my_cpc__cpc_write_timeout_new_arglist(fd, buf, n, 60, 0, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)cpc_write_timeout));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__linear_my_cpc__my_cpc__explicit_14_15_rc_85 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_14_15_rc_85_arglist *cpc_arguments ;
int my_cpc__n_81 ;
int my_cpc__ffd_82 ;
int my_cpc__fd_83 ;
char *my_cpc__buf_84 ;
int my_cpc__rc_80 ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__linear_my_cpc__my_cpc__explicit_14_15_rc_85_arglist ));
my_cpc__n_81 = (cpc_arguments -> my_cpc__n_81);
my_cpc__ffd_82 = (cpc_arguments -> my_cpc__ffd_82);
my_cpc__fd_83 = (cpc_arguments -> my_cpc__fd_83);
my_cpc__buf_84 = (cpc_arguments -> my_cpc__buf_84);
my_cpc__rc_80 = (cpc_arguments -> my_cpc__rc_80);
cpc_current_continuation = my_cpc__my_cpc__my_cpc__explicit_14_15_new_arglist(my_cpc__rc_80, my_cpc__n_81, my_cpc__ffd_82, my_cpc__fd_83, my_cpc__buf_84, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__my_cpc__explicit_14_15));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void my_cpc__my_cpc__explicit_12_13 (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__my_cpc__my_cpc__explicit_12_13_arglist *cpc_arguments ;
int i ;
int tmp ;
int ffd ;
char *fn ;
int rc ;
int fd ;
char *buf ;
int n ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__my_cpc__my_cpc__explicit_12_13_arglist ));
i = (cpc_arguments -> i);
tmp = (cpc_arguments -> tmp);
ffd = (cpc_arguments -> ffd);
fn = (cpc_arguments -> fn);
rc = (cpc_arguments -> rc);
fd = (cpc_arguments -> fd);
buf = (cpc_arguments -> buf);
n = (cpc_arguments -> n);
rc += tmp;
cpc_current_continuation = my_cpc__my_cpc__search_53_new_arglist(i, tmp, ffd, fn, rc, fd, n, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__search_53));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
void handle_connection (struct cpc_continuation *cpc_current_continuation ){
struct my_cpc__handle_connection_arglist *cpc_arguments ;
int fd ;
cpc_arguments = cpc_dealloc(cpc_current_continuation, sizeof(struct my_cpc__handle_connection_arglist ));
fd = (cpc_arguments -> fd);
int rc , n , ffd , len ;
char *buf , *fn ;
int i , val , tmp ;
val = 1;
rc = setsockopt(fd, 6, 1, ((char *)(& val)), sizeof(val));
if(rc < 0)
{
cpc_current_continuation = my_cpc__my_cpc__fail_52_new_arglist(fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__fail_52));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
buf = malloc(4096);
rc = 0;
cpc_current_continuation = my_cpc__my_cpc__again_39_new_arglist(len, fn, tmp, i, rc, n, ffd, fd, buf, cpc_current_continuation);
cpc_current_continuation = cpc_continuation_push(cpc_current_continuation, ((cpc_function *)my_cpc__again_39));
cpc_invoke_continuation(cpc_current_continuation);
return;
}
