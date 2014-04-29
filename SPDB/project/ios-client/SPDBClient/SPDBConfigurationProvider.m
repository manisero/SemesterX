#import "SPDBConfigurationProvider.h"

@implementation SPDBConfigurationProvider

+ (NSDictionary *)configurationProperties
{
    NSString *configPlist = [[NSBundle mainBundle] pathForResource:@"config" ofType:@"plist"];
    return [[NSDictionary alloc] initWithContentsOfFile:configPlist];
}

+ (NSString *)getServiceAddress
{
    return [[self configurationProperties] valueForKey:@"serviceAddress"];
}

@end
