struct sGtkMainLoopObject;
typedef struct sGtkMainLoopObject GtkMainLoopObject;
static GtkMainLoopObject *main_loop = NULL;

struct sNSObject;
typedef struct sNSObject NSObject;
static NSMainLoop *nsLoop = NULL;

int
main(void)
{
	main_loop = malloc (sizeof (GtkMainLoopObject));
	nsLoop = objc_object_new ("NSMainLoop", NULL);

	if (main_loop)
		run (main_loop);
	else
		run (nsLoop);

	return 0;
}
