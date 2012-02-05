#include "hssystemfileio-unix.h"

#include <errno.h>
#include <stdlib.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <unistd.h>

struct dirent *
hssystemfileio_alloc_dirent()
{
	return malloc(sizeof (struct dirent));
}

void
hssystemfileio_free_dirent(struct dirent *p)
{
	free(p);
}

int
hssystemfileio_readdir(DIR *dir, struct dirent *dirent)
{
	struct dirent *dirent_result;
	while (1)
	{
		int rc = readdir_r(dir, dirent, &dirent_result);
		if (rc != 0)
		{ return -1; }
		
		if (dirent_result == NULL)
		{ return 1; }
		
		return 0;
	}
}

char *
hssystemfileio_dirent_name(struct dirent *dirent)
{
	return dirent->d_name;
}

char *
hssystemfileio_getcwd(void)
{
#ifdef PATH_MAX
	int bufsize = PATH_MAX;
#else
	int bufsize = 4096;
#endif
	char *buf = malloc(bufsize);
	while (1)
	{
		char *ret = getcwd(buf, bufsize);
		if (ret != NULL)
		{ return ret; }
		
		free(buf);
		if (errno == ERANGE)
		{
			bufsize *= 2;
			buf = malloc(bufsize);
			continue;
		}
		return NULL;
	}
}

int
hssystemfileio_isrealdir(const char *path)
{
	struct stat st;
	int rc = lstat(path, &st);
	if (rc == -1)
	{ return rc; }
	
	if (S_ISDIR(st.st_mode))
	{ return 1; }
	
	return 0;
}

int
hssystemfileio_copy_permissions(const char *old_path, const char *new_path)
{
	struct stat st;
	int rc = stat(old_path, &st);
	if (rc == -1)
	{ return rc; }
	
	return chmod(new_path, st.st_mode);
}
