class Solution {
public:
    /**
     * @param n an integer
     * @return a list of pair<sum, probability>
     */


    vector<pair<int, double>> dicesSum(int n) {
        // Write your code here

        vector<vector<double> > D;
        D.reserve(n + 1);

        for(int i = 0; i <= n; ++i){
            D[i] = vector<double>(6*i + 1,0);
        }

        for(int i = 1; i <= 6; ++i){
            D[1][i] =1.0/6;
        }

        for(int i = 2; i <= n; ++i){
            for(int s = i; s <= 6*n; ++s){
                for(int j = 1; j <= 6; ++j){
                    if(s - j >= i - 1 && s - j <= 6*(i - 1)){
                        D[i][s] += D[i - 1][s - j]*(1.0/6);
                    }
                }
            }
        }

        vector<pair<int,double>> res;

        for(int i = n; i <= 6*n; ++i){
            res.push_back({i,D[n][i]});
        }

        return res;
    }
};
