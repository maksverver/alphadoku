#include <cstdlib>
#include <cstdio>
#include <cstring>
#include <iostream>

class Sudoku
{
protected:
    struct Node {
        struct Node *left, *right, *up, *down;
        struct Node *column;
        union data {
            int row;
            int size;
        } data;
    } *header;

    int *solution, solution_size;

    Node **rows;

    static void cover_column(Node *col);
    static void uncover_column(Node *col);
    int callback();

public:
    const int dim, size, area;

    Sudoku(int dim);
    ~Sudoku();

    void place(int p, int x);
    void clear();
    int search();
};

Sudoku::Sudoku(int dim) : dim(dim), size(dim*dim), area(size*size)
{
    // Initialize members
    const int cols = 4*size*size /* For diagonal sudoku: + 2*size */;
    header = new Node[1 + cols + size*cols];
    solution = new int[area];
    solution_size = 0;
    rows = new Node*[size*area];

    // Connect headers
    for(int n = 0; n < cols + 1; ++n)
    {
        header[n].left   = &header[(n + (cols + 1) - 1)%(cols + 1)];
        header[n].right  = &header[(n              + 1)%(cols + 1)];
        header[n].down   = &header[n];
        header[n].up     = &header[n];
        header[n].column = 0;
        header[n].data.size = 0;
    }
    header->data.size = 99999;

    // Connect one-nodes
    Node *base = header + 1 + cols;
    for(int row = 0; row < size*size*size; ++row)
    {
        rows[row] = base;

        int r = row/(size*size),
            c = (row/size)%size,
            b = dim*(r/dim) + c/dim,
            x = row%size;

        int cols[6], cols_size = 0;
        cols[cols_size++] = 1 + 0*size*size + row/size;
        cols[cols_size++] = 1 + 1*size*size + size*r + x;
        cols[cols_size++] = 1 + 2*size*size + size*c + x;
        cols[cols_size++] = 1 + 3*size*size + size*b + x;

        // For diagonal sudoku:
        // if(r == c)      cols[cols_size++] = 1 + 4*size*size + 0*size + x;
        // if(r + c == 8)  cols[cols_size++] = 1 + 4*size*size + 1*size + x;

        for(int n = 0; n < cols_size; ++n)
        {
            base[n].left   = &base[(n + cols_size - 1)%cols_size];
            base[n].right  = &base[(n             + 1)%cols_size];
            base[n].up     = header[cols[n]].up;
            base[n].down   = &header[cols[n]];
            base[n].up->down = &base[n];
            base[n].down->up = &base[n];

            base[n].data.row = row;

            base[n].column = &header[cols[n]];
            base[n].column->data.size += 1;
        }

        base += cols_size;
    }
}

Sudoku::~Sudoku()
{
    delete[] header;
    delete[] solution;
    delete[] rows;
}

void Sudoku::cover_column(Node *col)
{
    Node *i, *j;

    col->left->right = col->right;
    col->right->left = col->left;
    for(i = col->down; i != col; i = i->down)
        for(j = i->right; j != i; j = j->right)
        {
            j->down->up = j->up;
            j->up->down = j->down;
            --(j->column->data.size);
        }
}

void Sudoku::uncover_column(Node *col)
{
    Node *i, *j;
    for(i = col->up; i != col; i = i->up)
        for(j = i->left; j != i; j = j->left)
        {
            j->down->up = j->up->down = j;
            ++(j->column->data.size);
        }
    col->left->right = col->right->left = col;
}

int Sudoku::search()
{
    int result = 0;

    if(header->right == header)
    {
        result = callback();
    }
    else
    {
        /* Pick a column - using heuristic */
        Node *c = header->right;
        if(c->data.size > 1)
        {
            for(Node *j = header->right; j != header; j = j->right)
                if(j->data.size < c->data.size)
                {
                    c = j;
                    if(c->data.size <= 1)
                        break;
                }
        }

        if(c->data.size == 0)
            return result;

        /* Narrow solution on this column */
        cover_column(c);
        for(Node *r = c->down; r != c; r = r->down)
        {
            solution[solution_size++] = r->data.row;

            /* Cover row 'r' */
            for(Node *j = r->right; j != r; j = j->right)
                cover_column(j->column);

            /* Recurse.. */
            result = search();

            --solution_size;

            /* Uncover row 'r' */
            for(Node *j = r->left; j != r; j = j->left)
                uncover_column(j->column);

            if(result != 0)
                break;
        }
        uncover_column(c);
    }

    return result;
}

int Sudoku::callback()
{
    char str[area];
    for(int n = 0; n < area; ++n)
        str[solution[n]/size] = char(((dim <= 3) ? '1' : 'A') + solution[n]%size);
    fwrite(str, area, 1, stdout);
    fputc('\n', stdout);
    return 0;
}

void Sudoku::place(int p, int x)
{
    // Cover given row
    int row = size*p + x;
    solution[solution_size++] = row;

    for(Node *r = rows[row]; r->data.row == row; ++r)
        cover_column(r->column);
}

void Sudoku::clear()
{
    Node *rows = header + 1 + 4*size*size;
    while(solution_size > 0)
    {
        // Get last row selected
        int row = solution[--solution_size];

        // Uncover given row
        uncover_column(rows[4*row + 3].column);
        uncover_column(rows[4*row + 2].column);
        uncover_column(rows[4*row + 1].column);
        uncover_column(rows[4*row + 0].column);
    }
}

int main()
{
    // Dimension:
    // 3 for standard 9x9 sudoku
    // 5 for 25x25 alphadoku
    Sudoku sudoku(5);

    int count = 0;
    char str[8192];
    while(fgets(str, sizeof(str), stdin))
    {
        if(int(strlen(str)) < sudoku.area)
        {
            std::cerr << "Invalid input: " << str << std::endl;
            return 1;
        }

        ++count;

        for(int n = 0; n < sudoku.area; ++n)
        {
            if(str[n] != '.')
            {
                if(str[n] >= '1' && str[n] <= '9')
                    sudoku.place(n, str[n] - '1');
                else
                if(str[n] >= 'A' && str[n] <= 'Z')
                    sudoku.place(n, str[n] - 'A');
                else
                {
                    std::cerr << "Invalid input: " << str << std::endl;
                    return 1;
                }
            }
        }

        if(sudoku.search() == 0)
        {
	/*
            std::cerr << "Problem has no solution: " << str << std::endl;
            return 1;
	*/
        }

        sudoku.clear();
    }
    //std::cout << count << " problems processed!" << std::endl;
}
