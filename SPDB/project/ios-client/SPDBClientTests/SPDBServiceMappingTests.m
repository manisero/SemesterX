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

- (void)testMapEntryMapping
{
    SPDBMappingFactory *mappingFactory = [SPDBMappingFactory new];
    RKMapping *mapping = [mappingFactory createObjectMappingForMapEntry];
    id parsedJSON = [RKTestFixture parsedObjectWithContentsOfFixture:@"mapentry.json"];
    
    RKMappingTest *test = [RKMappingTest testForMapping:mapping sourceObject:parsedJSON destinationObject:nil];
    [test addExpectation:[RKPropertyMappingTestExpectation expectationWithSourceKeyPath:@"id" destinationKeyPath:@"id"]];
    [test addExpectation:[RKPropertyMappingTestExpectation expectationWithSourceKeyPath:@"wkt" destinationKeyPath:@"wkt"]];
    [test addExpectation:[RKPropertyMappingTestExpectation expectationWithSourceKeyPath:@"coordinates.latitude" destinationKeyPath:@"latitude"]];
    [test addExpectation:[RKPropertyMappingTestExpectation expectationWithSourceKeyPath:@"coordinates.longitude" destinationKeyPath:@"longitude"]];
    
    XCTAssertTrue([test evaluate], @"Mapping failed!");
}

@end
