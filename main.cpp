#include <iostream>
#include <string>
#include <vector>
#include <iomanip>
#include <cmath>

using namespace std;

class IdentityMatrix;
class SquareMatrix;
class ColumnVector{
public:
    vector<double> vectorr; //for vector b
    int m; //size of vector b

    ColumnVector(int m) {
        this->m = m;
        vectorr.resize(m);
    }

    //input overload operation
    friend istream& operator >> (istream& input, ColumnVector& vector1) {
        for (int i = 0; i < vector1.m; i++) {
            input >> vector1.vectorr[i];
        }
        return input;
    }

    //output overload operation
    friend ostream& operator << (ostream& output, ColumnVector& vector1) {
        output << fixed << setprecision(4);
        for (int i = 0; i < vector1.vectorr.size(); i++) {
            if(abs(vector1.vectorr[i] ) < pow(10, -10)){
                output << 0.00 << endl;
            } else {
                output << vector1.vectorr[i] << endl;
            }
        }
        return output;
    }
    ColumnVector operator + (ColumnVector& second) {
        ColumnVector result(m);
        for (int i = 0; i < m; i++) {
            result.vectorr[i] = vectorr[i] + second.vectorr[i];
        }
        return result;
    }

    ColumnVector operator * (ColumnVector& second) {
        double result;
        for (int i = 0; i < m; i++) {
            result+= vectorr[i] * second.vectorr[i];
        }
        return result;
    }

    ColumnVector operator * (double scalar) {
        ColumnVector result(m);
        for (int i = 0; i < m; i++) {
            result.vectorr[i] = vectorr[i] * scalar;
        }
        return result;
    }
    ColumnVector operator = (vector<double> source) const{
        ColumnVector result(m);
        if (source.size() == m){
            for (int i = 0; i < m; i ++){
                result.vectorr[i] = source[i];
            }
        }
        return result;
    }

    ColumnVector permutation(int i1, int i2){
        ColumnVector vector1(m);
        vector1.vectorr = vectorr;
        double temp = i1;
        vector1.vectorr[i1]= vectorr[i2];
        vector1.vectorr[i2] = temp;
        return vector1;
    }

};
class Matrix {
public:
    int n;
    int m;
    vector<vector<double>> matrix;
    Matrix (int n, int m){
        this->n = n;
        this->m = m;
        this->matrix.resize(n);
        for (int i = 0; i < n; i++) {
            matrix[i].resize(m);
        }
    }
    friend istream& operator >> (istream& input, Matrix& matrixInput) {
        for (int i = 0; i < matrixInput.n; i++) {
            for (int j = 0; j < matrixInput.m; j++) {
                input >> matrixInput.matrix[i][j];
            }
        }
        return input;
    }
    friend ostream& operator << (ostream& output, Matrix& matrixPrint) {
        output << fixed << setprecision(4);
        for (int i = 0; i < matrixPrint.n; i++) {
            for (int j = 0; j < matrixPrint.m; j++) {
                output << matrixPrint.matrix[i][j]<< " ";
            }
            endl(cout);
        }
        return output;
    }
    Matrix operator = (const Matrix& matrixTemp) const{
        Matrix matrix1(n, m);
        for (int i = 0; i < n; i++){
            for(int j = 0; j < m; j++){
                matrix1.matrix[i][j] = matrixTemp.matrix[i][j];
            }
        }
        return matrix1;
    }
    Matrix operator * (Matrix& matrixA){
        Matrix matrix1(n, matrixA.m);
        if(this->m == matrixA.n){
            for (int i = 0; i < n; i++){
                for (int k = 0; k < matrixA.m; k++) {
                    double sum = 0;
                    for(int j = 0; j < m; j++){
                        sum += (matrix[i][j] * matrixA.matrix[j][k]);
                    }
                    matrix1.matrix[i][k] = sum;
                }
            }
            return matrix1;
        } else{
            cout << "Error: the dimensional problem occurred" << endl;
            return Matrix(0, 0);
        }
    }
    Matrix operator * (ColumnVector& vector1) {
        Matrix result(n, 1);
        for (int i = 0; i < n; i++) {
            double sum = 0;
            for (int j = 0; j < m; j++){
                sum += vector1.vectorr[j] * matrix[i][j];
            }
            result.matrix[i][0] = sum;
        }
        return result;
    }
    Matrix transpose(){
        Matrix result(m, n);
        for (int i = 0; i < n; i++){
            for (int j = 0; j < m; j++){
                result.matrix[j][i] = matrix[i][j];
            }
        }
        return result;
    }

};

class SquareMatrix : public Matrix{
public:
    SquareMatrix(int n) : Matrix(n, n){}

};

class IdentityMatrix : public SquareMatrix{
public:
    IdentityMatrix(int n) : SquareMatrix(n){
        for (int i = 0; i <n ; i++){
            for (int j = 0 ; j < n; j ++){
                if (i == j){
                    matrix[i][j] = 1;
                }
            }
        }
    }
};
class EliminationMatrix : public SquareMatrix{
public:
    EliminationMatrix(int n) : SquareMatrix(n){}

    SquareMatrix elimination(int k, int m){
        SquareMatrix ans (n);
        double ratio;
        ratio = matrix[k][m] / matrix[m][m];
        for (int i = 0; i< n; i ++) {
            for (int j = 0; j < matrix.size(); j++) {
                if (i == j){
                    ans.matrix[i][j] = 1;
                }
            }
        }
        ans.matrix[k][m] = -ratio;
        return ans;
    }
};

class PermutationMatrix : public SquareMatrix{
public:
    PermutationMatrix(int n) : SquareMatrix(n){}

    SquareMatrix permutation(int i1, int i2){
        SquareMatrix ans(n);
        for (int k = 0; k < n; k++) {
            if (k == i1) {
                ans.matrix[k][i2] = 1;
            } else if (k == i2) {
                ans.matrix[k][i1] = 1;
            } else {
                ans.matrix[k][k] = 1;
            }
        }
        return ans;
    }
};
Matrix inverseMatrix(Matrix matrixAtransA){
    int a = matrixAtransA.m;
    IdentityMatrix identityMatrix(a);
    Matrix matrixA (a, 2*a);
    SquareMatrix result(a);
    for (int i = 0; i < a; i++){
        for (int j = 0; j <2*a; j++){
            if (j < a){
                matrixA.matrix[i][j] = matrixAtransA.matrix[i][j];
            }else{
                matrixA.matrix[i][j] = identityMatrix.matrix[i][j-a];
            }
        }
    }
    PermutationMatrix permutationMatrix(a);
    permutationMatrix.matrix = identityMatrix.matrix;
    int count = 0;
    int countPermut = 0;
    int pivot = 0;
    for (int j = 1; j < a; j++) {
        if (abs(matrixA.matrix[j][0]) > abs(matrixA.matrix[pivot][0])) {
            pivot = j;
        }
    }
    //Permutation
    if(pivot!=0){
        SquareMatrix pPerm = permutationMatrix.permutation(0, pivot);
        permutationMatrix.matrix = pPerm.matrix;
        Matrix pA = matrixA;
        pA.matrix = (permutationMatrix * pA).matrix;
        matrixA.matrix = pA.matrix;
        count+=1;
        countPermut+=1;
    }
    for(int i=0;i<a;i++) {
        //Elimination
        for(int j=i+1; j<a;j++){
            if (abs(matrixA.matrix[j][i]) != 0) {
                EliminationMatrix eliminationMatrix(a);
                eliminationMatrix.matrix = matrixA.matrix;
                SquareMatrix pElim = eliminationMatrix.elimination(j,i);
                eliminationMatrix.matrix = pElim.matrix;
                Matrix pA = matrixA;
                pA.matrix = (eliminationMatrix * matrixA).matrix;
                matrixA.matrix = pA.matrix;
                count+=1;
            }
        }
        for (int k = 0; k < a; k++) {
            pivot = k;
            for (int j = k + 1; j < a; j++) {
                if (abs(matrixA.matrix[j][k]) > abs(matrixA.matrix[pivot][k])) {
                    pivot = j;
                }
            }
            //Permutation
            if(pivot!=k){
                SquareMatrix pPerm = permutationMatrix.permutation(k, pivot);
                permutationMatrix.matrix = pPerm.matrix;
                Matrix pA = matrixA;
                pA.matrix = (permutationMatrix * pA).matrix;
                matrixA.matrix = pA.matrix;
                count+=1;
                countPermut+=1;
            }
        }
    }
    for(int i=a-1;i>=0;i--) {
        //Elimination
        for (int j = i - 1; j >= 0; j--) {
            if (abs(matrixA.matrix[j][i]) != 0) {
                EliminationMatrix eliminationMatrix(a);
                eliminationMatrix.matrix = matrixA.matrix;
                SquareMatrix pElim = eliminationMatrix.elimination(j, i);
                eliminationMatrix.matrix = pElim.matrix;
                Matrix pA = matrixA;
                pA.matrix = (eliminationMatrix * matrixA).matrix;
                matrixA.matrix = pA.matrix;
                count += 1;
            }
        }
    }
    for (int i =0; i < a; i++){
        for ( int j = 0; j < a; j ++){
            if (i == j){
                double x = matrixA.matrix[i][j] / 1;
                for (int k = 0; k < a; k++){
                    matrixA.matrix[i][k+a] = matrixA.matrix[i][k+a] / x;
                }
                matrixA.matrix[i][j] = 1;
            }
        }
    }
    for (int i =0; i < a; i ++){
        for ( int j = 0; j < a; j ++){
            result.matrix[i][j] = matrixA.matrix[i][j+a];
        }
    }
    return result;
}

int main() {
    int m, work, degree;
    cin >> m;
    vector<double> vectort;
    vector<double> vectorb;
    for (int i = 0; i < m; i++){
        cin >> work;
        vectort.push_back(work);
        cin >>work;
        vectorb.push_back(work);
    }
    ColumnVector vectorT (m);
    ColumnVector vectorB (m);
    vectorT = (vectorT = vectort);
    vectorB = (vectorB = vectorb);
    cin >> degree;
    cout << "A:"<<endl;
    Matrix matrixA(m, degree+1);
    for (int i = 0; i < m; i ++){
        matrixA.matrix[i][0] = 1;
        for (int j = 1; j <= degree; j++){
            matrixA.matrix[i][j] = pow(vectorT.vectorr[i],j);
        }
    }
    cout << matrixA;
    cout<<"A_T*A:"<<endl;
    Matrix matrixAtr = matrixA.transpose();
    Matrix matrixAtrA = matrixAtr*matrixA;
    cout <<matrixAtrA;
    cout<<"(A_T*A)^-1:" <<endl;
    Matrix matrixInverse = inverseMatrix(matrixAtrA);
    cout<<matrixInverse;
    cout<< "A_T*b:"<<endl;
    Matrix matrixAtrB = matrixAtr * vectorB;
    cout<<matrixAtrB;
    cout<<"x~:"<<endl;
    Matrix answer = matrixInverse * matrixAtrB;
    cout << answer;

    return 0;
}
