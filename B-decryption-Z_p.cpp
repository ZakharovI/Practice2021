#include <iostream>
#include <vector>
#include <algorithm>
#define numtype unsigned long long

using namespace std;

const string ALP = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz .";

numtype char_to_int(char ch) {
    if (ch >= '0' && ch <= '9')
        return int(ch - '0');
    if (ch >= 'A' && ch <= 'Z')
        return int(ch - 'A') + 10;
    if (ch >= 'a' && ch <= 'z')
        return int(ch - 'a') + 10 + 26;
    if (ch == ' ')
        return 62;
    if (ch == '.')
        return 63;
    return 64;
}
char int_to_char(numtype dig) {
    if (0 <= dig && dig <= 9) {
        return char(int('0') + dig);
    }
    if (10 <= dig && dig <= 35) {
        return char(int('A') + dig - 10);
    }
    if (36 <= dig && dig <= 61) {
        return char(int('a') + dig - 36);
    }
    if (dig == 62) {
        return ' ';
    }
    return '.';
}


char module64(vector<numtype> &v, numtype p) {
    numtype ans = 0;
    for (numtype i = 0; i < (numtype)v.size(); ++i) {
        ans *= p;
        ans += v[i];
        ans %= 64;
    }
    return int_to_char(ans);
}

void divide64(vector<numtype> &v, numtype p) {
    numtype i = 0;
    numtype cur10 = 0;
    vector<numtype> res;
    while (i < (numtype)v.size()) {
        cur10 *= p;
        cur10 += v[i];
        if (cur10 >= 64) {
            res.push_back(cur10 / 64);
            cur10 = cur10 % 64;
        }
        else {
            if (res.size() > 0) {
                res.push_back(0);
            }
        }
        i++;
    }
    v = res;
}

void conv_p_to_64(vector<numtype> &v, numtype p, string &ans) {
    while (!v.empty() && !(v.size() == 1 && v[0] == 0)) {
        ans += module64(v, p);
        divide64(v, p);
    }
    while (!ans.empty() && ans[ans.size() - 1] == '0') {
        ans.pop_back();
    }
}


numtype pow(numtype a, numtype b, numtype p) {
    if (b == 0) {
        return 1;
    }
    if (b == 1) {
        return a % p;
    }
    if (b % 2 == 0) {
        return pow((a * a) % p, b / 2, p);
    }
    else {
        return (a * pow((a * a) % p, b / 2, p)) % p;
    }
}

int main() {
    numtype p, k;
    cin >> p >> k;
    vector<numtype> a, b;
    numtype ai, bi;
    while (cin >> ai >> bi) {
        a.push_back(ai);
        b.push_back(bi);
    }

    vector<numtype> messagep(a.size());
    for (numtype i = 0; i < (numtype)a.size(); ++i) {
        messagep[i] = (b[i] * pow(a[i], p - 1 - k, p)) % p;
    }
    reverse(messagep.begin(), messagep.end());

    string message64 = "";
    conv_p_to_64(messagep, p, message64);
    cout << message64;
    return 0;
}
