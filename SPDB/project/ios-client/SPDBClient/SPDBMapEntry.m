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
