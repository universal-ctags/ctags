//
//  commentary test
//  SupaView
//
//  Created by Vincent Berthoux on 14/06/10.
//  Copyright 2010 __MyCompanyName__. All rights reserved.
//

#import <Cocoa/Cocoa.h>
#import "LayoutTree.h"

@class LayoutTree;
@class FolderTree;

#define       A_MACRO_TEST

typedef something SampleTypedefObjC;

// Mer & no_struct_name must not be present in output
// tag
 # define ANOTHER_MACRO( WITH, MOAR ) \
                Mer( ) \
                struct no_struct_name

struct aStruct
{
    int aStructMember;
    char *anotherStructMember[ NOT_IN_TAG ];
};

#pragma DONTCARE /* :-) */

@interface FileTree : NSObject {
	NSString	*name;
    LayoutTree  *representation;
    FolderTree  *parent[THISISNOTATAG];
    FileSize    diskSize;
}
- (id)initWithName:(NSString*)treeName
           andSize:(uint64_t)size
           atPlace:(FolderTree*)parentFolder;

- (id)initWithName:(NSString*)treeName
           atPlace:(FolderTree*)parentFolder;

- (void)dealloc;

- (FileSize)getDiskSize;
- (LayoutTree*)createLayoutTree;
@end

@interface FolderTree : FileTree {
    NSMutableArray     *children;
}

- (id)initWithName:(NSString*)treeName
           atPlace:(FolderTree*)parentFolder;
- (void)dealloc;

- (FolderTree*)addChild:(FileTree*)subTree;
- (void) populateChildList:(NSString*)root;
- (LayoutTree*)createLayoutTree;
@end

@interface NSString (Extension)
- (void)doSomething;
@end

@interface NSString (Extension2) <Proto2>
- (void)doSomething2;
@end

@interface NSString (Extension34) <Proto3, Proto4>
- (void)doSomething34;
@end

@interface MyString <Proto5>
- (void)doSomething5;
@end

@interface YourString <Proto6, Proto7>
- (void)doSomething67;
@end
