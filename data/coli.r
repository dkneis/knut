coli <- utils::read.table(header=TRUE, sep="\t", text='
time	density	id_experiment	id_strain	id_replicate
0.1	-7.73E-04	25C_SSR_K_4g	Fs Lw UP1-7	1
0.3	2.27E-04	25C_SSR_K_4g	Fs Lw UP1-7	1
0.6	2.27E-04	25C_SSR_K_4g	Fs Lw UP1-7	1
0.8	2.27E-04	25C_SSR_K_4g	Fs Lw UP1-7	1
1.1	1.23E-03	25C_SSR_K_4g	Fs Lw UP1-7	1
1.3	2.23E-03	25C_SSR_K_4g	Fs Lw UP1-7	1
1.6	2.23E-03	25C_SSR_K_4g	Fs Lw UP1-7	1
1.8	4.23E-03	25C_SSR_K_4g	Fs Lw UP1-7	1
2.1	6.23E-03	25C_SSR_K_4g	Fs Lw UP1-7	1
2.3	1.02E-02	25C_SSR_K_4g	Fs Lw UP1-7	1
2.6	1.42E-02	25C_SSR_K_4g	Fs Lw UP1-7	1
2.8	2.12E-02	25C_SSR_K_4g	Fs Lw UP1-7	1
3.1	3.02E-02	25C_SSR_K_4g	Fs Lw UP1-7	1
3.3	4.22E-02	25C_SSR_K_4g	Fs Lw UP1-7	1
3.6	5.32E-02	25C_SSR_K_4g	Fs Lw UP1-7	1
3.8	5.82E-02	25C_SSR_K_4g	Fs Lw UP1-7	1
4.1	6.92E-02	25C_SSR_K_4g	Fs Lw UP1-7	1
4.3	8.72E-02	25C_SSR_K_4g	Fs Lw UP1-7	1
4.6	1.03E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
4.8	1.26E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
5.1	1.46E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
5.3	1.62E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
5.6	1.89E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
5.8	2.87E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
6.1	3.28E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
6.3	3.36E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
6.6	3.28E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
6.8	3.82E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
7.1	4.08E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
7.3	4.15E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
7.6	4.29E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
7.8	4.27E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
8.1	4.44E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
8.3	4.63E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
8.6	4.61E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
8.8	4.80E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
9.1	4.97E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
9.3	5.20E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
9.6	5.39E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
9.8	5.53E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
10.1	5.75E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
10.3	5.92E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
10.6	6.17E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
10.8	6.50E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
11.1	6.62E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
11.3	6.80E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
11.6	7.16E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
11.8	7.21E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
12.1	7.53E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
12.3	7.76E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
12.6	8.21E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
12.8	8.61E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
13.1	8.85E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
13.3	9.08E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
13.6	9.54E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
13.8	9.81E-01	25C_SSR_K_4g	Fs Lw UP1-7	1
14.1	1.00E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
14.3	1.03E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
14.6	1.06E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
14.8	1.10E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
15.1	1.12E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
15.3	1.16E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
15.6	1.18E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
15.8	1.20E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
16.1	1.22E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
16.3	1.25E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
16.6	1.26E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
16.8	1.29E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
17.1	1.31E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
17.3	1.32E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
17.6	1.34E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
17.8	1.35E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
18.1	1.38E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
18.3	1.39E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
18.6	1.40E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
18.8	1.41E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
19.1	1.42E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
19.3	1.43E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
19.6	1.44E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
19.8	1.45E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
20.1	1.45E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
20.3	1.46E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
20.6	1.47E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
20.8	1.48E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
21.1	1.49E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
21.3	1.48E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
21.6	1.49E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
21.8	1.50E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
22.1	1.51E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
22.3	1.51E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
22.6	1.52E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
22.8	1.52E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
23.1	1.52E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
23.3	1.53E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
23.6	1.53E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
23.8	1.53E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
24.1	1.54E+00	25C_SSR_K_4g	Fs Lw UP1-7	1
0.1	2.27E-04	25C_SSR_K_4g	Fs Lw UP1-7	2
0.3	1.23E-03	25C_SSR_K_4g	Fs Lw UP1-7	2
0.6	1.23E-03	25C_SSR_K_4g	Fs Lw UP1-7	2
0.8	2.23E-03	25C_SSR_K_4g	Fs Lw UP1-7	2
1.1	1.23E-03	25C_SSR_K_4g	Fs Lw UP1-7	2
1.3	3.23E-03	25C_SSR_K_4g	Fs Lw UP1-7	2
1.6	2.23E-03	25C_SSR_K_4g	Fs Lw UP1-7	2
1.8	4.23E-03	25C_SSR_K_4g	Fs Lw UP1-7	2
2.1	6.23E-03	25C_SSR_K_4g	Fs Lw UP1-7	2
2.3	9.23E-03	25C_SSR_K_4g	Fs Lw UP1-7	2
2.6	1.42E-02	25C_SSR_K_4g	Fs Lw UP1-7	2
2.8	2.12E-02	25C_SSR_K_4g	Fs Lw UP1-7	2
3.1	3.12E-02	25C_SSR_K_4g	Fs Lw UP1-7	2
3.3	4.52E-02	25C_SSR_K_4g	Fs Lw UP1-7	2
3.6	5.32E-02	25C_SSR_K_4g	Fs Lw UP1-7	2
3.8	5.92E-02	25C_SSR_K_4g	Fs Lw UP1-7	2
4.1	8.22E-02	25C_SSR_K_4g	Fs Lw UP1-7	2
4.3	9.42E-02	25C_SSR_K_4g	Fs Lw UP1-7	2
4.6	1.10E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
4.8	1.38E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
5.1	1.65E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
5.3	1.76E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
5.6	1.93E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
5.8	2.69E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
6.1	3.04E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
6.3	3.00E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
6.6	2.81E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
6.8	3.58E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
7.1	3.79E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
7.3	3.72E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
7.6	3.93E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
7.8	3.89E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
8.1	4.17E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
8.3	4.38E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
8.6	4.32E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
8.8	4.55E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
9.1	4.79E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
9.3	4.96E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
9.6	5.06E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
9.8	5.30E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
10.1	5.57E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
10.3	5.43E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
10.6	5.87E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
10.8	6.04E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
11.1	6.26E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
11.3	6.45E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
11.6	6.65E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
11.8	6.91E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
12.1	7.22E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
12.3	7.62E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
12.6	7.91E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
12.8	8.24E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
13.1	8.36E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
13.3	8.53E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
13.6	8.97E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
13.8	9.23E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
14.1	9.30E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
14.3	9.49E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
14.6	9.83E-01	25C_SSR_K_4g	Fs Lw UP1-7	2
14.8	1.01E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
15.1	1.05E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
15.3	1.08E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
15.6	1.09E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
15.8	1.12E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
16.1	1.13E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
16.3	1.15E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
16.6	1.18E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
16.8	1.20E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
17.1	1.23E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
17.3	1.23E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
17.6	1.26E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
17.8	1.28E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
18.1	1.28E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
18.3	1.31E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
18.6	1.32E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
18.8	1.33E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
19.1	1.33E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
19.3	1.35E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
19.6	1.36E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
19.8	1.36E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
20.1	1.38E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
20.3	1.40E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
20.6	1.41E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
20.8	1.42E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
21.1	1.43E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
21.3	1.43E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
21.6	1.44E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
21.8	1.45E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
22.1	1.46E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
22.3	1.46E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
22.6	1.46E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
22.8	1.47E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
23.1	1.48E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
23.3	1.49E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
23.6	1.49E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
23.8	1.50E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
24.1	1.51E+00	25C_SSR_K_4g	Fs Lw UP1-7	2
')
