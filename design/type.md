Type system design of SysY2022.

类型系统是针对表达式设计的，Stmt本身没有类型，或者说是`()`。另外函数也没有类型，因为SysY语言定义并没有指针和函数指针。

# Formation

$$
\vdash\mathrm{int}\ \mathrm{type}
$$
$$
\vdash\mathrm{float}\ \mathrm{type}
$$
$$
\cfrac{\alpha\ \mathrm{type}\quad N:\mathrm{int}\quad N\ge 0}{\alpha[N]\ \mathrm{type}}
$$

$$
\vdash\mathrm{int}[]\ \mathrm{type}
\qquad
\vdash\mathrm{float}[]\ \mathrm{type}
$$

大写表示编译期常量。第二种情况对应函数参数第一维无长度的情况。

# Literals

$$
\vdash N : \mathrm{int},\ \text{if } N \in \mathbb N \text{ and } -2^{31}\le N \le 2^{31}-1
$$

$$
\vdash F : \mathrm{float},\ \text{if } -2^{128}\le F \le 2^{128}
$$


# Indexing

$$
\cfrac{v : \alpha[N_1]\cdots[N_n] \qquad i_1\cdots i_n : \mathrm{int}}{v[i_1]\cdots[v_n]:\alpha}
$$
$$
\cfrac{v : \alpha[N_1]\cdots[N_n] \qquad i_1\cdots i_m : \mathrm{int}}{v[i_1]\cdots[v_m]:\alpha[N_{m+1}]\cdots[N_n]},\ n \ne m
$$

$$
\cfrac{v : \alpha[][N_2]\cdots[N_n] \qquad i_1\cdots i_n : \mathrm{int}}{v[i_1]\cdots[v_n]:\alpha}
$$
$$
\cfrac{v : \alpha[][N_2]\cdots[N_n] \qquad i_1\cdots i_m : \mathrm{int}}{v[i_1]\cdots[v_m]:\alpha[N_{m+1}]\cdots[N_n]},\ n \ne m
$$




# Operator

$$
\mathrm{arith\_op} := \text{+ | - | * | /}
$$

$$
\mathrm{comp\_op} := \text{< | > | <= | >= | == | !=}
$$

$$
\mathrm{cond\_op} := \text{'\&\&' | '||'}
$$

$$
\cfrac{\circ \in \mathrm{cond\_op}\cup\mathrm{comp\_op}\cup\mathrm{arith\_op} \qquad a : \mathrm{int} \quad b : \mathrm{int}}{a \circ b : \mathrm{int}}
$$

$$
\cfrac{\circ \in \mathrm{arith\_op} \qquad a : \mathrm{float} \quad b : \mathrm{float}}{a \circ b : \mathrm{float}}
$$

$$
\cfrac{\circ \in \mathrm{comp\_op} \qquad a : \mathrm{float} \quad b : \mathrm{float}}{a \circ b : \mathrm{int}}
$$

$$
\cfrac{a : \mathrm{int} \quad b : \mathrm{int}}{a \text{ \% } b : \mathrm{int}}
$$

$$
\cfrac{a : \mathrm{int}}{!a: \mathrm{int}}
\quad
\cfrac{a : \mathrm{int}}{+a: \mathrm{int}}
\quad
\cfrac{a : \mathrm{int}}{-a: \mathrm{int}}
$$

$$
\cfrac{a : \mathrm{float}}{+a: \mathrm{float}}
\quad
\cfrac{a : \mathrm{float}}{-a: \mathrm{float}}
$$

隐含转换的情况(int2float)：

$$
\cfrac{\circ \in \mathrm{arith\_op} \qquad a : \mathrm{int} \quad b : \mathrm{float}}{a \circ b : \mathrm{float}}
\qquad
\cfrac{\circ \in \mathrm{arith\_op} \qquad a : \mathrm{float} \quad b : \mathrm{int}}{a \circ b : \mathrm{float}}
$$

$$
\cfrac{\circ \in \mathrm{comp\_op} \qquad a : \mathrm{int} \quad b : \mathrm{float}}{a \circ b : \mathrm{int}}
\qquad
\cfrac{\circ \in \mathrm{comp\_op} \qquad a : \mathrm{float} \quad b : \mathrm{int}}{a \circ b : \mathrm{int}}
$$

