class Solution {
public:
    /**
     * @param n: the rows of matrix
     * @param m: the cols of matrix
     * @param badcomputers: the bad computers
     * @return: The answer
     */
    int maintenance(int n, int m, vector<Point> &bc) {

       //A tree with keys = rows, and values = a tree of columns
       map<int,set<int> > mp;

       //insert all the points to the tree
       for(auto &p : bc)
         mp[p.x].insert(p.y);

        //A N*2 table - per row we keep the min distance to its end
        // and min distance to its begining
        vector<vector<long long> > dp(n,vector<long long>(2,INT_MAX));

        dp[0][1] = m - 1;
        dp[0][0] = 2*(*prev(mp[0].end()));

        for(auto i = next(mp.begin()); i != mp.end(); ++i){

            auto &s = i->second;

            //previous row ended at it begining this row will end at its begining
            //  (i->first - (prev(i)->first)) -> is the number of columns we had to skip to get
            // from previous row to this row
            long long endbegfrombeg =  (i->first - (prev(i)->first))  + dp[prev(i)->first][0] + 2*(*prev(s.end()));

           //previous row ended at its end and this row ends at its begining
            long long endbegfromlast = (i->first - (prev(i)->first)) + dp[prev(i)->first][1] +  m - 1;

            //the exact opposite of the previous two
            long long endlastfrombeg = (i->first - (prev(i)->first)) + dp[prev(i)->first][0] + m - 1;
            long long endlastfromlast = (i->first - (prev(i)->first)) + dp[prev(i)->first][1] + 2*(m - 1 - *s.begin());

            //set the minimum for begining and end of this row
            dp[i->first][0] = min(endbegfrombeg,endbegfromlast);
            dp[i->first][1] = min(endlastfrombeg,endlastfromlast);
        }

        //the index of the last row
        int last = prev(mp.end())->first;

        //the minimum result of the last row with a broken computer + the distance to the last row
        return min(dp[last][1],dp[last][0]) + (n - 1 - last);
    }
};
