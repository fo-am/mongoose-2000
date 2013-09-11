#ifndef APP_H_INCLUDED
#define APP_H_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

extern void appInit();
extern void appDeinit();
extern void appEval(char *code);

/* Value is non-zero when application is alive, and 0 when it is closing.
 * Defined by the application framework.
 */
extern int gAppAlive;


#ifdef __cplusplus
}
#endif


#endif // !APP_H_INCLUDED
