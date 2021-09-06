#include <iostream>
#include <vector>
#include <algorithm>
#include <string>
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


numtype modulep(string &s, numtype p) {
    numtype ans = 0;
    for (numtype i = 0; i < (numtype)s.size(); ++i) {
        ans *= 64;
        ans += char_to_int(s[i]);
        ans %= p;
    }
    return ans;
}


void dividep(string &s, numtype p) {
    numtype i = 0;
    numtype cur10 = 0;
    string res = "";
    while (i < (numtype)s.size()) {
        cur10 *= 64;
        cur10 += char_to_int(s[i]);
        if (cur10 >= p) {
            res += int_to_char(cur10 / p);
            cur10 = cur10 % p;
        }
        else {
            if (res.size() > 0) {
                res += '0';
            }
        }
        i++;
    }
    if (res == "") {
        res = "0";
    }
    s = res;
}


void conv_64_to_p(string &s, numtype p, vector<numtype> &ans) {
    while (s != "0") {
        ans.push_back(modulep(s, p));
        dividep(s, p);
        //cerr << '"' << s << '"' << '\n';
    }
}



int main() {

    numtype p, g, y;
    cin >> p >> g >> y;

    string message64;
    ws(cin);
    getline(cin, message64);

    reverse(message64.begin(), message64.end());
    /// big-endian

    vector<numtype> messagep;
    conv_64_to_p(message64, p, messagep);

    numtype gk, yk, b;
    for (numtype M : messagep) {
        gk = (g * g) % p;
        yk = (y * y) % p;
        b = (yk * M) % p;
        cout << gk << ' ' << b << '\n';
    }

    return 0;
}
