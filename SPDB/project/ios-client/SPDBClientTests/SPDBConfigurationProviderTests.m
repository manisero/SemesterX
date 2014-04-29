#import "SPDBConfigurationProvider.h"
#import <XCTest/XCTest.h>

@interface SPDBConfigurationProviderTests : XCTestCase

@end

@implementation SPDBConfigurationProviderTests

- (void)setUp
{
    [super setUp];
}

- (void)tearDown
{
    [super tearDown];
}

- (void)testGetServiceAddressMethod
{
    NSString *serviceAddress = [SPDBConfigurationProvider getServiceAddress];
    
    XCTAssertTrue([serviceAddress isEqualToString:@"http://localhost:8080"], @"Service address does not match!");
}

@end
