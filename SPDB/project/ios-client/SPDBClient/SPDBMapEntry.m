//
//  SPDBMapEntry.m
//  SPDBClient
//
//  Created by Jakub Turek on 29.04.2014.
//  Copyright (c) 2014 Jakub Turek. All rights reserved.
//

#import "SPDBMapEntry.h"

@implementation SPDBMapEntry

+ (id)mapEntryWithLatitude:(NSNumber *)latitude andLongitude:(NSNumber *)longitude
{
    SPDBMapEntry *mapEntry = [SPDBMapEntry new];
    
    mapEntry.latitude = latitude;
    mapEntry.longitude = longitude;
    
    return mapEntry;
}

@end
