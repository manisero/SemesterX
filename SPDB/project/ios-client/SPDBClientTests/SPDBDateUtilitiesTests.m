#import "SPDBDateUtilities.h"
#import <XCTest/XCTest.h>

@interface SPDBDateUtilitiesTests : XCTestCase

@end

@implementation SPDBDateUtilitiesTests

- (void)setUp
{
    [super setUp];
}

- (void)tearDown
{
    [super tearDown];
}

- (void)testStripSecondsFromDateMethod
{
    NSDateFormatter *dateFormatter = [NSDateFormatter new];
    [dateFormatter setDateFormat:@"yyyy-MM-dd HH:mm:ss"];
    NSDate *date = [dateFormatter dateFromString:@"2014-05-01 14:17:33"];
    NSDate *expectedDate = [dateFormatter dateFromString:@"2014-05-01 14:17:00"];
    SPDBDateUtilities *dateUtilities = [SPDBDateUtilities new];
    
    NSDate *strippedDate = [dateUtilities stripSecondsFromDate:date];
    
    XCTAssertTrue([expectedDate isEqualToDate:strippedDate]);
}

- (void)testSubtractSecondsFromDateMethod
{
    NSDateFormatter *dateFormatter = [NSDateFormatter new];
    [dateFormatter setDateFormat:@"yyyy-MM-dd HH:mm:ss"];
    NSDate *date = [dateFormatter dateFromString:@"2014-05-01 15:00:00"];
    NSDate *expectedDate = [dateFormatter dateFromString:@"2014-05-01 14:55:00"];
    SPDBDateUtilities *dateUtilities = [SPDBDateUtilities new];
    
    NSDate *calculatedDate = [dateUtilities subtractSeconds:300 fromDate:date];
    
    XCTAssertTrue([expectedDate isEqualToDate:calculatedDate]);
}

@end
