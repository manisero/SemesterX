#import "SPDBShortestPathFetcher.h"
#import "SPDBShortestPathFetcherDelegate.h"
#import <XCTAsyncTestCase/XCTAsyncTestCase.h>

@interface SPDBShortestPathFetcherTests : XCTAsyncTestCase<SPDBShortestPathFetcherDelegate>

@property (nonatomic, strong) NSArray *shortestPath;

@end

@implementation SPDBShortestPathFetcherTests

- (void)setUp
{
    [super setUp];
}

- (void)tearDown
{
    [super tearDown];
}

- (void)testFetchShortestPathFromEntryMethod
{
    SPDBShortestPathFetcher *shortestPathFetcher = [SPDBShortestPathFetcher new];
    SPDBMapEntry *entryFrom = [SPDBMapEntry mapEntryWithLatitude:[NSNumber numberWithDouble:52.220067] andLongitude:[NSNumber numberWithDouble:21.012119]];
    SPDBMapEntry *entryTo = [SPDBMapEntry mapEntryWithLatitude:[NSNumber numberWithDouble:52.230014] andLongitude:[NSNumber numberWithDouble:21.011886]];
    
    [self prepare];
    
    [shortestPathFetcher fetchShortestPathFromEntry:entryFrom toEntry:entryTo delegate:self];
    
    [self waitForStatus:kXCTUnitWaitStatusSuccess timeout:10.0];
    
    XCTAssertNotNil(self.shortestPath, @"No mapping result found");
    XCTAssertEqual([self.shortestPath count], 5, @"Response mapping failed");
    XCTAssertEqualObjects([NSNumber numberWithDouble:52.220067], [self.shortestPath[0] latitude], @"Latitude is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:21.012119], [self.shortestPath[0] longitude], @"Longitude is not right!");
    XCTAssertEqualObjects(@"POINT( 52.22006700 21.01211900 )", [self.shortestPath[0] wkt], @"Wkt is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:52.219893], [self.shortestPath[1] latitude], @"Latitude is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:21.018152], [self.shortestPath[1] longitude], @"Longitude is not right!");
    XCTAssertEqualObjects(@"POINT( 52.21989300 21.01815200 )", [self.shortestPath[1] wkt], @"Wkt is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:52.223232], [self.shortestPath[2] latitude], @"Latitude is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:21.015984], [self.shortestPath[2] longitude], @"Longitude is not right!");
    XCTAssertEqualObjects(@"POINT( 52.22323200 21.01598400 )", [self.shortestPath[2] wkt], @"Wkt is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:52.226229], [self.shortestPath[3] latitude], @"Latitude is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:21.014161], [self.shortestPath[3] longitude], @"Longitude is not right!");
    XCTAssertEqualObjects(@"POINT( 52.22622900 21.01416100 )", [self.shortestPath[3] wkt], @"Wkt is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:52.230014], [self.shortestPath[4] latitude], @"Latitude is not right!");
    XCTAssertEqualObjects([NSNumber numberWithDouble:21.011886], [self.shortestPath[4] longitude], @"Longitude is not right!");
    XCTAssertEqualObjects(@"POINT( 52.23001400 21.01188600 )", [self.shortestPath[4] wkt], @"Wkt is not right!");
}

- (void)updateProgress:(CGFloat)progress
{
}

- (void)didFetchShortestPath:(NSArray *)shortestPath
{
    self.shortestPath = shortestPath;
    [self notify:kXCTUnitWaitStatusSuccess];
}

- (void)didFailFetchingShortestPath:(NSError *)error
{
    [self notify:kXCTUnitWaitStatusFailure];
}

@end
