//
//  SPDBServiceMappingTests.m
//  SPDBClient
//
//  Created by Jakub Turek on 29.04.2014.
//  Copyright (c) 2014 Jakub Turek. All rights reserved.
//

#import <RestKit/RestKit.h>
#import <RestKit/Testing.h>
#import "SPDBMappingFactory.h"
#import <XCTest/XCTest.h>

@interface SPDBServiceMappingTests : XCTestCase

@end

@implementation SPDBServiceMappingTests

- (void)setUp
{
    [super setUp];

    NSBundle *testTargetBundle = [NSBundle bundleWithIdentifier:@"pl.edu.pw.elka.spdb.SPDBClientTests"];
    [RKTestFixture setFixtureBundle:testTargetBundle];
}

- (void)tearDown
{
    [super tearDown];
}

- (RKMappingTest *)mapEntryMappingTest
{
    SPDBMappingFactory *mappingFactory = [SPDBMappingFactory new];
    RKMapping *mapping = [mappingFactory createObjectMappingForMapEntry];
    id parsedJSON = [RKTestFixture parsedObjectWithContentsOfFixture:@"mapentry.json"];
    
    return [RKMappingTest testForMapping:mapping sourceObject:parsedJSON destinationObject:nil];
}

- (void)testIdMapping
{
    XCTAssertTrue([[self mapEntryMappingTest] evaluateExpectation:[RKPropertyMappingTestExpectation expectationWithSourceKeyPath:@"id" destinationKeyPath:@"id" value:[NSNumber numberWithLong:0]] error:nil], @"Id mapping failed!");
}

- (void)testWktMapping
{
    XCTAssertTrue([[self mapEntryMappingTest] evaluateExpectation:[RKPropertyMappingTestExpectation expectationWithSourceKeyPath:@"wkt" destinationKeyPath:@"wkt" value:@"POINT( 52.23173000 21.00595200 )"] error:nil], @"Wkt mapping failed!");
}

- (void)testLatitudeMapping
{
    XCTAssertTrue([[self mapEntryMappingTest] evaluateExpectation:[RKPropertyMappingTestExpectation expectationWithSourceKeyPath:@"coordinates.latitude" destinationKeyPath:@"latitude" value:[NSNumber numberWithDouble:52.231730]] error:nil], @"Latitude mapping failed!");
}

- (void)testLongitudeMapping
{
    XCTAssertTrue([[self mapEntryMappingTest] evaluateExpectation:[RKPropertyMappingTestExpectation expectationWithSourceKeyPath:@"coordinates.longitude" destinationKeyPath:@"longitude" value:[NSNumber numberWithDouble:21.005952]] error:nil], @"Longitude mapping failed!");
}

@end
