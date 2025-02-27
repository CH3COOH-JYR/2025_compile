#include <stdio.h>
int getint()
{
    int n;
    scanf("%d", &n);
    return n;
}
const int b = 6;
const int b_1 = 6, b_2 = 7, b_3 = 8;
const int a[1] = {1};
const int a_1[1] = {1}, a_2[1] = {1}, a_3[1] = {1};
int c;
int c_1, c_2, c_3;
int d = 6;
int d_1 = 6, d_2 = 7, d_3 = 8;
int h[1] = {1};
int h_1[1] = {1}, h_2[1] = {1}, h_3[1] = {1};
void func_1() { return; }
void func_2(int a) { return; }
void func_3(int a, int b, int c) { return; }
int func_4() { return 0; }
int func_5(int a) { return 0; }
int func_6(int a, int b, int c) { return 0; }
void func_7() {}
void func_8()
{
    int aa;
    aa = 1;
    return;
    int bb;
    bb = aa;
}
void func_9()
{
    int aa = 1;
    if (aa == 1)
        return;
}
void func_10()
{
    int aa = 1;
    if (aa == 2)
        return;
    else
        aa = 2;
    return;
}
void func_11()
{
    int aa = 1;
    while (aa == 1)
        aa = 2;
    return;
}
void func_12()
{
    int aa = 1;
    while (aa == 1)
        break;
    return;
}
void func_13()
{
    int aa = 1;
    while (aa == 1)
    {
        aa = 2;
        continue;
    }
    return;
}
int func_14()
{
    int aa = 1;
    return aa;
}
int func_15()
{
    int aa;
    aa = getint();
    return aa;
}
void func_16()
{
    printf("1");
    return;
}
void func_17()
{
    int aa = 1;
    printf("%d", aa);
    return;
}
void func_18()
{
    int aa, bb, cc;
    aa = 3;
    bb = 2;
    cc = +3;
    cc = -3;
    if (!cc)
        cc = 5;
    cc = aa + bb;
    cc = aa - bb;
    cc = aa * bb;
    cc = aa / bb;
    cc = aa % bb;
    if (aa > bb)
        cc = 1;
    if (aa < bb)
        cc = 2;
    if (aa >= bb)
        cc = 3;
    if (aa <= bb)
        cc = 4;
    if (aa != bb)
        cc = 5;
    return;
}
void func_19()
{
    int aa, bb, cc;
    aa = 1;
    bb = 1;
    if (aa && bb)
        cc = 1;
    if (aa || bb)
        cc = 1;
    return;
}
int main()
{
    func_2(1);
    ;
    func_3(1, 2, 3);
    h[0] = 2;
    h[0]=(b)+(d);
    printf("2023202303");
    printf("1");
    printf("1");
    printf("1");
    printf("1");
    printf("1");
    printf("1");
    func_17();
    printf("1");
    func_16();
    return 0;
}