
@implementation FileTree
- (FileSize)getDiskSize
{
    return diskSize;
}

- (id)initWithName:(NSString*)treeName
           atPlace:(FolderTree*)parentFolder
{
    self = [super init];

    diskSize = 0;
    name = treeName;
    parent = parentFolder;
    [name retain];
    representation = nil;

    return self;
}

- (id)initWithName:(NSString*)treeName
           andSize:(FileSize)size
           atPlace:(FolderTree*)parentFolder
{
    self = [super init];

    diskSize = size;
    name = treeName;
    parent = parentFolder;
    [name retain];
    representation = nil;

    return self;
}

- (void)dealloc
{
    [name release];
    [representation release];
    [super dealloc];
}

- (LayoutTree*)createLayoutTree
{
    return nil;
}
@end

@implementation FolderTree
- (id)initWithName:(NSString*)treeName
           atPlace:(FolderTree*)parentFolder
{
    self = [super initWithName:treeName
                       atPlace:parentFolder];

    children = [[NSMutableArray alloc] init];
    return self;
}

- (void)dealloc
{
    [children release];
    [super dealloc];
}

+ (void) createFileList: (NSString*)root atPlace:(FolderTree*)parentFolder
{
	NSFileManager *localFileManager = [[NSFileManager alloc] init];
	NSURL		  *rootUrl = [NSURL fileURLWithPath:root];
	NSDirectoryEnumerator *dirEnumerator = [localFileManager enumeratorAtURL:rootUrl
											
                                                  includingPropertiesForKeys:[NSArray arrayWithObjects:
                                                                              NSURLNameKey,
                                                                              NSURLIsDirectoryKey,
                                                                              nil]
											
                                                                     options:NSDirectoryEnumerationSkipsHiddenFiles
											
                                                                errorHandler:nil];
    
	for (NSURL *theURL in dirEnumerator)
	{
        [theURL getResourceValue:&fileName
						  forKey:NSURLNameKey
						   error:NULL];
        
        // Ignore files under the _extras directory
        if ([isDirectory boolValue]==YES)
        {
            [folder populateChildList:root];
        }
        else if ([isDirectory boolValue]==NO)
        {
            [parentFolder addChild:f];
        }		
    }
}

- (void) populateChildList:(NSString*)root
{
    NSString *thisRoot = [[root stringByAppendingString:@"/"]
                                stringByAppendingString:name];
    
    [FolderTree createFileList:thisRoot
                       atPlace:self];
    
    for ( FileTree *f in children )
        diskSize += [f getDiskSize];
}

- (FolderTree*)addChild:(FileTree*)subTree
{
    [children addObject:subTree];
    return self;
}

- (LayoutTree*)createLayoutTree
{
    return
        [[LayoutTree alloc] initWithFileList:children
                                andTotalSize:diskSize];
}
@end

