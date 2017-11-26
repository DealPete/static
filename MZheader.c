// Print details of a MS-DOS .EXE file header.

#include <stdio.h>
#include <string.h>

struct DOS_Header 
{
    char signature[2];
    short lastsize;
    short nblocks;
    short nreloc;
    short hdrsize;
    short minalloc;
    short maxalloc;
    short ss;
    short sp;
    short checksum;
    short ip;
    short cs;
    short relocpos;
    short noverlay;
    short reserved1[4];
    short oem_id;
    short oem_info;
    short reserved2[10];
    long  e_lfanew;
};

int main(int argc, char *argv[]) {
    FILE *exefile;
    struct DOS_Header header;

    if (argc == 1) {
	printf("Usage: a.out [EXEFILE]/n");
	return 1;
    }

    exefile=fopen(argv[1], "rb");
    
    if (!exefile) {
		printf("Can't read file!");
		return 1;
    }

    fread(&header, sizeof(struct DOS_Header), 1, exefile);
    
	int exe_start = 16*header.hdrsize;
	int entry_point = exe_start + 16*header.cs;

    if (!strcmp(header.signature, "MZ")) {
		printf("File should start with Mark Zbikowski's initials. This is not a DOS .EXE file.");
	}

	printf("Number of blocks: %d\n", header.nblocks);
    printf("Number of entries in relocation table: %d\n", header.nreloc);
    printf("Header size: %d bytes\n", 16*header.hdrsize);
    
    printf("Stack segment: %xh\n", header.ss);
    printf("Stack pointer: %xh\n", header.sp);
    printf("Instruction pointer: %xh\n", header.ip);
    printf("Code segment: %xh\n\n", header.cs);

	printf("EXE data starts at %xh\n", exe_start);
	printf("Entry point at %xh\n", entry_point);
}
