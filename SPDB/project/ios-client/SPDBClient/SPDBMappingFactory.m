//
//  SPDBMappingFactory.m
//  SPDBClient
//
//  Created by Jakub Turek on 29.04.2014.
//  Copyright (c) 2014 Jakub Turek. All rights reserved.
//

#import "SPDBMapEntry.h"
#import "SPDBMappingFactory.h"

@implementation SPDBMappingFactory

- (RKObjectMapping *)createObjectMappingForMapEntry
{
    RKObjectMapping *mapping = [RKObjectMapping mappingForClass:[SPDBMapEntry class]];
    [mapping addAttributeMappingsFromDictionary:@{
        @"id":                      @"id",
        @"wkt":                     @"wkt",
        @"coordinates.latitude":    @"latitude",
        @"coordinates.longitude":   @"longitude"
    }];
    
    return mapping;
}

@end
