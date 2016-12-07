int main()
{
        if (something) {
                printf("hello");
#ifdef world
        } else
                printf(" world\n");
#else
        }
#endif
}

int func()
{
}
