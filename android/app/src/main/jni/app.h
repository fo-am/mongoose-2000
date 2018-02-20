#ifndef APP_H_INCLUDED
#define APP_H_INCLUDED
#ifdef __cplusplus
extern "C" {
#endif

extern void appInit();
extern void appDeinit();
extern void appEval(const char *code);

#ifdef __cplusplus
}
#endif


#endif // !APP_H_INCLUDED
