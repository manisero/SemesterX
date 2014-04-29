//
//  SPDBMappingFactory.h
//  SPDBClient
//
//  Created by Jakub Turek on 29.04.2014.
//  Copyright (c) 2014 Jakub Turek. All rights reserved.
//

#import <Foundation/Foundation.h>
#import <RestKit/RestKit.h>

@interface SPDBMappingFactory : NSObject

- (RKObjectMapping *)createObjectMappingForMapEntry;

@end
